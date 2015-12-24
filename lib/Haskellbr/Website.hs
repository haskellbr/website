{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Exposes a spock application running the HaskellBR website
module Haskellbr.Website
  ( module Haskellbr.Website
  ) where

import           Control.Monad                 (forM)
import           Control.Monad.IO.Class        (MonadIO)
import qualified Data.ByteString.Lazy          as ByteString
import           Data.Functor.Identity
import           Data.Monoid
import           Lucid
import           Network.Wai.Middleware.Static (hasPrefix, staticPolicy)
import           System.Environment            (getArgs)
import           Web.Spock

-- |
-- Renders Lucid and sends it as the response
renderLucid :: MonadIO m => Html a1 -> ActionT m ()
renderLucid = lazyBytes . renderBS

-- |
-- Main entry-point for the application
main :: IO ()
main = getArgs >>= \case
    ("serve":_) -> runSpock 3000 . spockT id $ do
        get "/" $ renderLucid homepage
        -- Serve files on the static dir
        middleware (staticPolicy (hasPrefix "static"))
    _ -> do
        putStrLn $
          "Defaulting to generating the index.html file that is the " <>
          "whole HaskellBR homepage"
        ByteString.writeFile "./index.html" (renderBS homepage)
        putStrLn $ "Wrote file to ./index.html"

-- * Templates

homepage :: HtmlT Identity ()
homepage = wrapper $ do
    _ <- div_ [ class_ "homepage-header" ] $ do
        img_ [ class_ "logo" , src_ "/static/images/haskellbr-logo.jpg" ]
        ul_ [ class_ "homepage-links" ] $ do
            let links = [ ("Blog", "http://blog.haskellbr.com/")
                        , ("GitHub", "https://github.com/haskellbr")
                        , ("Twitter", "https://twitter.com/haskellbr2")
                        , ("Lista de e-mails", "https://mail.haskell.org/mailman/listinfo/haskell-br")
                        , ("Canal #haskell-br no freenode", "http://irc.lc/freenode/haskell-br")
                        , ("Slack", "http://haskellbr.com/slack/")
                        , ("Google+", "https://plus.google.com/communities/114632834967823295855")
                        , ("HaskellBR-SP no Meetup", "http://www.meetup.com/haskellbr-sp/")
                        ]
            forM links $ \(name, href) ->
                li_ [] $ a_ [ class_ "btn btn-primary"
                            , href_ href
                            ] name

    div_ [ class_ "homepage-events center-block" ] $ do
        h2_ "Próximos eventos"
        p_ [] $ do
            "Os meetups da HaskellBR em São Paulo estão sendo organizados pelo Garoa"
            "Hacker Clube. Wiki oficial em: "
            a_ [ href_ "https://garoa.net.br/wiki/Haskell_Meetup"
               ] "https://garoa.net.br/wiki/Haskell_Meetup"
            "."
        ul_ [ class_ "center-block events" ] $
            li_ [ class_ "event" ] $ do
                span_ [ class_ "event-date"
                      ]
                    "25/01/2016"
                br_ []
                a_ [ href_ "https://plus.google.com/events/cojvbaipbp62v10fhh4q8ki1apc" ] $
                    h3_ [ class_ "event-name" ] "7º Encontro de Haskellers de São Paulo"
                a_ [ href_ "http://www.meetup.com/haskellbr-sp/events/227526368/" ]
                    "Evento no Meetup"
                br_ []
                a_ [ href_ "https://plus.google.com/events/cojvbaipbp62v10fhh4q8ki1apc" ]
                    "Google plus"
                br_ []
                span_ [ class_ "event-location" ] $
                    a_ [ href_ "https://goo.gl/maps/s6WrbWADUyo" ]
                        "Garoa Hacker Clube (Rua Costa Carvalho, 567 - Pinheiros. CEP 05429-130)"

wrapper :: Monad m => HtmlT m a -> HtmlT m a
wrapper content = doctypehtml_ $ do
    head_ $ do
        title_ "HaskellBR - Comunidade Brasileira de usários de Haskell"

        meta_ [ charset_ "UTF-8" ]
        meta_ [ name_ "author", content_ "Pedro Tacla Yamada" ]
        meta_ [ name_ "description"
              , content_ ( "Site oficial da comunidade de Haskell no brazil. "
                        <> "A HaskellBR é uma comunidade formada com o "
                        <> "intuito de aprender mais sobre a linguagem "
                        <> "de programação Haskell e seu ecossistema, "
                        <> "bem como produzir conteúdo e contribuições "
                        <> "para a comunidade brasileira e internacional."
                         )
              ]
        meta_ [ name_ "viewport"
              , content_ "width=device-width,initial-scale=1"
              ]
        meta_ [ name_ "google-site-verification"
              , content_ "lFBpphPRu2gJ5X87prcSkI6w7ALeC0ijvJw-uEF2aIk"
              ]
        link_ [ href_ "/static/css/homepage.css"
              , rel_ "stylesheet"
              , type_ "text/css"
              ]
        link_ [ href_ "/static/css/bootstrap.css"
              , rel_ "stylesheet"
              , type_ "text/css"
              ]
        link_ [ href_ "https://fonts.googleapis.com/css?family=Ubuntu:400,300,700|Ubuntu+Mono:400,700"
              , rel_ "stylesheet"
              , type_ "text/css"
              ]
        link_ [ href_ "/static/images/favicon.ico"
              , rel_ "shortcut icon"
              ]
    body_ content
