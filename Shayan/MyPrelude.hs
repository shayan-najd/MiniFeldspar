module MyPrelude 
       (Int,Integer,Num(..),div,toInteger,
        Bool(..),(&&),(||),not,
        IO,print,readFile,writeFile,
        fst,snd,
        String,lines,unlines,
        read,
        (.),flip,curry,uncurry,id,const,
        (++),zip,head,dropWhile,iterate,length,zipWith,filter,(!!),
        Maybe(..),fromJust,maybe,
        Enum(..),
        Ord(..),
        Eq(..),
        Show(..),
        otherwise, impossible,
        lookup,
        State,getState,put,modify,runState,execState,evalState,
        module ErrorMonad,
        module Existential,
        module Data.Monoid,
        module Data.Foldable, 
        module Data.Traversable,
        module Data.Functor,
        module Control.Applicative,
        module Control.Monad,
        module Data.Array)
       where
import Existential
import Data.Maybe
import ErrorMonad
import Data.Array
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Data.Functor
import Control.Monad hiding (forM,forM_,sequence,sequence_,msum,mapM,mapM_)
import Data.Monoid
import Control.Monad.State 

impossible :: a
impossible = error "Impossible!"

getState :: MonadState s m => m s
getState = get