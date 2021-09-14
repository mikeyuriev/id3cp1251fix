{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Exception (handle)
import Control.Monad (unless, when)
import Data.Char (isSpace)
import Data.Encoding (decodeString)
import Data.Encoding.CP1251 (CP1251(..))
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import Data.Text as Text (Text, pack, unpack)
import Text.Printf (printf)
import Sound.HTagLib
import System.IO (hFlush, stdout)
import System.Directory.Glob (globMany, globDefaults, globMark)
import System.Environment (getArgs, getProgName)

data Error
  = CantReadTagsErr
  | NoTagsErr

data TagData = TagData
  { _title   :: Title
  , _artist  :: Artist
  , _album   :: Album
  , _comment :: Comment
  , _genre   :: Genre
  }

data TrackData = TrackData
  { path    :: FilePath
  , title   :: ( Text, Text )
  , artist  :: ( Text, Text )
  , album   :: ( Text, Text )
  , comment :: ( Text, Text )
  , genre   :: ( Text, Text )
  }

data ConfirmOption
  = ExitConfirmOpt
  | DecodeAllConfirmOpt
  | DecodeConfirmOpt
  | SkipConfirmOpt

main :: IO ()
main = do
  getArgs >>= parseArgs

parseArgs :: [ String ] -> IO ()
parseArgs ("-y":globs) = process globs True
parseArgs ("-h":_)     = help
parseArgs globs@(_:_)  = process globs False
parseArgs _            = do
  progName <- getProgName
  putStrLn $ progName ++ ": missing argument"
  putStrLn $ "Try '" ++ progName ++ " -h' for more info"

help :: IO ()
help = do
  progName <- getProgName
  printf   "Usage: %s [-y] [FILE]...\n" progName
  printf   "  or:  %s -h\n" progName
  putStrLn "Decode CP1251 ID3 tags to UTF8"
  putStrLn ""
  putStrLn "  -h - display this help and exit"
  putStrLn "  -y - decode without confirmation"
  putStrLn ""
  putStrLn "FILE arguments supports wildcards like '/home/user/music/*.mp3'"

process :: [ String ] -> Bool -> IO ()
process globs yes = do
  paths <- filter (not . isSuffixOf "/") <$> globMany (globDefaults <> globMark) globs
  tracks <- catMaybes <$> mapM loadTrackData paths
  unless (null tracks) $ do
    when (length tracks > 1) $ mapM_ showTrackData tracks
    if yes
      then processTracks tracks True
      else if length tracks == 1
        then processTracks tracks False
        else choice tracks ""
  where
    choice tracks "a" = processTracks tracks True
    choice tracks "c" = putStrLn ("\n" ++ replicate 80 '-') >> processTracks tracks False
    choice _ "e"      = return ()
    choice tracks _   = do
      putStr "Decode (a)ll, (c)onfirm each or (e)xit? "
      hFlush stdout
      getLine >>= choice tracks

processTracks :: [ TrackData ] -> Bool -> IO ()
processTracks [] _ = return ()
processTracks tracks True = mapM_ saveTrackData tracks
processTracks [track] _ = do
  showTrackData track
  choice ""
  where
    choice :: String -> IO ()
    choice "s" = return ()
    choice "d" = saveTrackData track
    choice _ = do
      putStr "(d)ecode or (s)kip? "
      hFlush stdout
      getLine >>= choice
processTracks tracks@(track:rest) _ = do
  showTrackData track
  choice ""
  where
    choice :: String -> IO ()
    choice "e" = return ()
    choice "d" = saveTrackData track >> processTracks rest False
    choice "a" = processTracks tracks True
    choice "s" = processTracks rest False
    choice _   = do
      putStr "(d)ecode, (s)kip, decode (a)ll or (e)xit? "
      hFlush stdout
      getLine >>= choice

showTrackData :: TrackData -> IO ()
showTrackData TrackData{ path, title, artist, album, comment, genre } = do
  putStrLn ""
  putStrLn $ "file:    " ++ path
  putEntry "title:" title
  putEntry "artist:" artist
  putEntry "album:" album
  putEntry "comment:" comment
  putEntry "genre:" genre
    where
      putEntry :: Text -> ( Text , Text ) -> IO ()
      putEntry header entry =
        let
          ( o, d ) = entry
        in
          putStrLn
            $ replaceCtls
            $ printf "%-8.8s %s -> %s" header (tab 32 $ unpack o) (tab 32 $ unpack d)

saveTrackData :: TrackData -> IO ()
saveTrackData TrackData{ path, title, artist, album, comment, genre } =
  handle exceptionHandler $ do
    putStr $ path ++ " ... "
    hFlush stdout
    setTags path (Just ID3v2UTF8) $
      titleSetter (mkTitle $ snd title) <>
      artistSetter (mkArtist $ snd artist) <>
      albumSetter (mkAlbum $ snd album) <>
      commentSetter (mkComment $ snd comment) <>
      genreSetter (mkGenre $ snd genre)
    putStrLn "ok"
  where
    exceptionHandler :: HTagLibException -> IO ()
    exceptionHandler _ = putStrLn "failed"

loadTrackData :: FilePath -> IO (Maybe TrackData)
loadTrackData path = handle exceptionHandler $ do
  d <- getTags path tagDataGetter
  let title' = unTitle (_title d)
  let artist' = unArtist (_artist d)
  let album' = unAlbum (_album d)
  let comment' = unComment (_comment d)
  let genre' = unGenre (_genre d)
  return $ Just TrackData
    { path = path
    , title = ( title', decode title' )
    , artist = ( artist', decode artist' )
    , album = ( album', decode album' )
    , comment = ( comment', decode comment' )
    , genre = ( genre', decode genre' )
    }
  where
    exceptionHandler :: HTagLibException -> IO (Maybe TrackData)
    exceptionHandler _ = do
      putStrLn $ "Loading failed: " ++ path
      return Nothing

tab :: Int -> String -> String
tab width s
  | width <= 3        = error "width must be greater than 3"
  | length s > width  = printf "%-*.*s..." (width - 3) (width - 3) s
  | otherwise         = printf "%-*.*s" width width s

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

replaceCtls :: String -> String
replaceCtls = map (\c -> if fromEnum c > 31 then c else '.')

decode :: Text -> Text
decode = pack . decodeString CP1251 . unpack

tagDataGetter :: TagGetter TagData
tagDataGetter = TagData
  <$> titleGetter
  <*> artistGetter
  <*> albumGetter
  <*> commentGetter
  <*> genreGetter
