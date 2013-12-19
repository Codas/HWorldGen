module FR.Region (readWorld, towns, roads) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Matrix           as M

import           FR.Poi
import           FR.Points

readWorld :: IO (Matrix Int)
readWorld = do
    bs <- BS.readFile "terrain.txt"
    let rows  = C8.split '\n' bs
        llist = map (C8.split ',') rows
        m     = fromLists (map (map (read . C8.unpack)) llist :: [[Int]])
    return m


highmoon = T { tLoc = np (FP 211 243)
             , tName = "Highmoon"
             , tPop = 8000 }
ordulin = T { tLoc = np (FP 375 275)
            , tName = "Ordulin"
            , tPop = 36300 }
archenbridge = T { tLoc = np (FP 228 320)
                 , tName = "Archenbridge"
                 , tPop = 8000 }
daerlun = T { tLoc = np (FP 184 433)
             , tName = "Daerlun"
             , tPop = 52477 }
urmlaspyr = T { tLoc = np (FP 220 455)
              , tName = "Urmlaspyr"
              , tPop = 18000 }
saerloon = T { tLoc = np (FP 300 382)
             , tName = "Saerloon"
             , tPop = 54496 }
selgaunt = T { tLoc = np (FP 353 357)
             , tName = "Selgaunt"
             , tPop = 56514 }
yhaunn = T { tLoc = np (FP 438 254)
           , tName = "Yhaunn"
           , tPop = 25000 }
arabel = T { tLoc = np (FP 13 305)
           , tName = "Arabel"
           , tPop = 30600 }
scar = T { tLoc = np (FP 13 305)
         , tName = "Tilverton Scar"
         , tPop = 50 }
ashabenford = T { tLoc = np (FP 216 130)
                , tName = "Ashabenford"
                , tPop = 455 }
chandlerscross = T { tLoc = np (FP 471 196)
                   , tName = "Chandlerscross"
                   , tPop = 5303 }

towns = [highmoon, ordulin, archenbridge, daerlun, urmlaspyr, saerloon,
         selgaunt, yhaunn, arabel, scar, ashabenford, chandlerscross]

roads = [ arabel    <~> highmoon
        , arabel    <~> ashabenford
        , highmoon  <~> ordulin
        , ordulin   <~> yhaunn
        , ordulin   <~> archenbridge
        , yhaunn    <~> selgaunt
        , selgaunt  <~> saerloon
        , urmlaspyr <~> saerloon
        , daerlun   <~> saerloon
        , daerlun   <~> urmlaspyr
        , daerlun   <~> archenbridge ]
