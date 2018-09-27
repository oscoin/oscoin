module Oscoin.CLI.Spinner
    ( Theme
    , Progress
    , Spinner
    , defaultTheme
    , themes
    , withSpinner
    , withThemedSpinner
    , newTestSpinner
    , progress
    ) where

import           Oscoin.Prelude

import           Control.Concurrent (myThreadId)
import           Data.List.NonEmpty ((!!))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import           System.Console.ANSI (clearLine, hideCursor, setCursorColumn)
import           System.IO
                 (BufferMode(..), Handle, hGetBuffering, hSetBuffering, stdout)

-- | A 'Theme' is a 'NonEmpty' list of textual steps to be rendered
-- in sequence by a 'Spinner', wrapping around to the beginning when
-- the end is reached.
type Theme = NonEmpty Text

-- | 'Progress' text is displayed by the 'Spinner' on the left of its indicator.
type Progress = Text

-- | A 'Spinner' is a terminal UI component that continuously renders
-- a 'Theme'ed progress indicator on stdout and an optional matching 'Progress'
-- text on its left.
data Spinner = Spinner
    { spinnerTheme    :: Theme
    , spinnerProgress :: MVar Progress
    , spinnerInterval :: Int
    , spinnerThreadId :: ThreadId
    }

-- | The default 'Theme' used by 'withSpinner'.
defaultTheme :: Theme
defaultTheme = NonEmpty.head themes

-- | All the 'Theme's provided by this module to be used in 'withThemedSpinner'.
-- Other themes can be instantiated and used ad-hoc.
themes :: NonEmpty Theme
themes = NonEmpty.fromList $ NonEmpty.fromList <$>
    [ ["   ", ".  ", ".. ", "...", ".. ", ".  "]
    , ["|", "/", "-", "\\"]
    , ["◴", "◷", "◶", "◵"]
    , ["←", "↑", "→", "↓"]
    , ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
    ]

-- | Like 'withThemedSpinner' but with 'defaultTheme' as the theme.
withSpinner :: (MonadIO m, MonadMask m) => Progress -> Int -> (Spinner -> m a) -> m a
withSpinner = withThemedSpinner defaultTheme

-- | Spawns a Spinner in a separate thread and passes it to the supplied function 'f'.
-- The provided 'theme' is rendered from its first step along with the initial 'prg'.
-- The 'interval' defines the time to sleep between each rendering step in milliseconds.
withThemedSpinner :: (MonadIO m, MonadMask m) => Theme -> Progress -> Int -> (Spinner -> m a) -> m a
withThemedSpinner theme prg interval f =
    bracket (start theme prg interval) stop f

-- | Sets the progress text to be displayed by the 'Spinner' thread.
progress :: MonadIO m => Spinner -> Progress -> m ()
progress Spinner{..} = void . liftIO . swapMVar spinnerProgress

newTestSpinner :: MonadIO m => m Spinner
newTestSpinner = liftIO $ do
    prg <- newEmptyMVar
    Spinner defaultTheme prg 0 <$> myThreadId

start :: MonadIO m => Theme -> Progress -> Int -> m Spinner
start theme prg interval = liftIO $ do
    prg' <- newMVar prg
    tid  <- myThreadId
    let s = Spinner theme prg' (interval * 1000) tid
    tid' <- forkIO $ hWithBufferMode stdout NoBuffering (spin s 0)
    pure s { spinnerThreadId = tid' }

spin :: MonadIO m => Spinner -> Int -> m ()
spin spinner@Spinner{..} i = liftIO $ do
    prg <- maybe mempty identity <$> tryReadMVar spinnerProgress
    render prg $ step spinnerTheme len i
    threadDelay spinnerInterval
    spin spinner (i + 1)
    where len = length spinnerTheme

step :: Theme -> Int -> Int -> Text
step theme len iteration = theme !! (iteration `mod` len)

render :: MonadIO m => Progress -> Text -> m ()
render prg indicator = liftIO $ do
    clearLine
    hideCursor
    setCursorColumn 0
    putStr $ T.stripEnd $ T.concat [prg, " ", indicator]

stop :: MonadIO m => Spinner -> m ()
stop Spinner{..} = liftIO $ do
    prg <- maybe mempty identity <$> tryReadMVar spinnerProgress
    killThread spinnerThreadId
    render prg $ NonEmpty.head spinnerTheme
    putStr ("\n" :: Text)

-- | Performs an IO action with some buffer mode on a handle
hWithBufferMode :: (MonadIO m, MonadMask m) => Handle -> BufferMode -> m a -> m a
hWithBufferMode handle bufferMode action = do
    originalBuffering <- liftIO $ hGetBuffering handle
    bracket_
        (liftIO (hSetBuffering handle bufferMode))
        (liftIO (hSetBuffering handle originalBuffering))
        action
