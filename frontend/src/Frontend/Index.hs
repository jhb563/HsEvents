{-# LANGUAGE OverloadedStrings #-}

module Frontend.Index where

import Reflex.Dom

runIndex :: IO ()
runIndex = mainWidget $ el "div" $ text "Welcome to Reflex!"
