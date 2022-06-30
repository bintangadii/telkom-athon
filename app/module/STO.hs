module Module.STO where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)

data LogSTO
    = LogSTO
        { stoId :: Int
        , stoName :: String
        }
    | UnknownSTO
    deriving (Show, Eq)