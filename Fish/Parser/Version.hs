{-# language TemplateHaskell #-}
module Fish.Parser.Version where

import Development.GitRev

version :: String
version = $(gitHash)
