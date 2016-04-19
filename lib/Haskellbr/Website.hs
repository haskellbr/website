{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Exposes a spock application running the HaskellBR website
module Haskellbr.Website
  ( module Haskellbr.Website
  ) where

import           Control.Monad                 (forM, forM_)
import           Control.Monad.IO.Class        (MonadIO)
import qualified Data.ByteString.Lazy          as ByteString
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Text                     (Text)
import           Lucid
import           Network.Wai                   (Application)
import           Network.Wai.Middleware.Static (hasPrefix, staticPolicy)
import           System.Environment            (getArgs)
import           Web.Spock

-- |
-- Renders Lucid and sends it as the response
renderLucid :: MonadIO m => Html a1 -> ActionT m ()
renderLucid = lazyBytes . renderBS

app :: IO Application
app = spockAsApp $ spockT id $ do
    get "/" $ renderLucid homepage
    -- Serve files on the static dir
    middleware (staticPolicy (hasPrefix "static"))

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
        putStrLn "Wrote file to ./index.html"

-- * Templates

homepage :: HtmlT Identity ()
homepage = wrapper $ do
    _ <- nav_ [ class_ "homepage-links navbar navbar-default" ] $
        div_ [ class_ "container" ] $
            ul_ [ class_ "nav navbar-nav" ] $ do
                let links = [ ("Blog", "http://blog.haskellbr.com/")
                            , ("GitHub", "https://github.com/haskellbr")
                            , ("Twitter", "https://twitter.com/haskellbr2")
                            , ("Lista de e-mails", "https://mail.haskell.org/mailman/listinfo/haskell-br")
                            , ("Canal #haskell-br no freenode", "http://irc.lc/freenode/haskell-br")
                            , ("Slack", "http://slack.haskellbr.com")
                            , ("Google+", "https://plus.google.com/communities/114632834967823295855")
                            , ("HaskellBR-SP no Meetup", "http://www.meetup.com/haskellbr-sp/")
                            ]
                forM links $ \(name, href) ->
                    li_ [] $ a_ [ href_ href
                                ] name

    _ <- div_ [ class_ "homepage-header" ] $
        img_ [ class_ "logo" , src_ "/static/images/haskellbr-logo.jpg" ]

    div_ [ class_ "homepage-events center-block" ] $ do
        h2_ $ do
            "Próximos eventos de FP"
            br_ []
            "(não só Haskell)"
        ul_ [ class_ "center-block events" ] $
            event EventData { eventDate = "25/01/2016"
                            , eventTitle = "7º Encontro de Haskellers de São Paulo"
                            , eventLinks = [ ("Evento no Meetup", "http://www.meetup.com/haskellbr-sp/events/227526368/a")
                                           , ("Google plus", "https://plus.google.com/events/cojvbaipbp62v10fhh4q8ki1apc")
                                           ]
                            , eventLink = "https://plus.google.com/events/cojvbaipbp62v10fhh4q8ki1apc"
                            , eventLocation = ("Garoa Hacker Clube (Rua Costa Carvalho, 567 - Pinheiros. CEP 05429-130)", "https://goo.gl/maps/s6WrbWADUyo")
                            }

    div_ [ class_ "homepage-events center-block" ] $ do
        h2_ "Últimos eventos"
        ul_ [ class_ "center-block events" ] $
            event EventData { eventDate = "25/01/2016"
                            , eventTitle = "7º Encontro de Haskellers de São Paulo"
                            , eventLinks = [ ("Evento no Meetup", "http://www.meetup.com/haskellbr-sp/events/227526368/a")
                                           , ("Google plus", "https://plus.google.com/events/cojvbaipbp62v10fhh4q8ki1apc")
                                           ]
                            , eventLink = "https://plus.google.com/events/cojvbaipbp62v10fhh4q8ki1apc"
                            , eventLocation = ("Garoa Hacker Clube (Rua Costa Carvalho, 567 - Pinheiros. CEP 05429-130)", "https://goo.gl/maps/s6WrbWADUyo")
                            }

data EventData = EventData { eventTitle    :: Html ()
                           , eventDate     :: Html ()
                           , eventLinks    :: [(Html (), Text)]
                           , eventLink     :: Text
                           , eventLocation :: (Html (), Text)
                           }

event :: EventData -> HtmlT Identity ()
event eventData = li_ [ class_ "event" ] $ do
    span_ [ class_ "event-date"
          ]
        (eventDate eventData)
    br_ []
    a_ [ href_ (eventLink eventData) ] $
        h3_ [ class_ "event-name" ] (eventTitle eventData) -- "7º Encontro de Haskellers de São Paulo"
    forM_ (eventLinks eventData) $ \(linkName, link) -> do
        a_ [ href_ link ] linkName
        br_ []
    br_ []
    span_ [ class_ "event-location" ] $ do
        _ <- a_ [ href_ (snd (eventLocation eventData)) ] -- "https://goo.gl/maps/s6WrbWADUyo" ]
            (fst (eventLocation eventData)) -- "Garoa Hacker Clube (Rua Costa Carvalho, 567 - Pinheiros. CEP 05429-130)"
        return ()

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
