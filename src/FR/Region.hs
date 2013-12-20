module FR.Region (readWorld, towns, roads) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Matrix           as M

import           FR.Poi
import           FR.Points
import           FR.Terrain

readWorld :: IO (Matrix Int)
readWorld = do
    bs <- BS.readFile "terrain.txt"
    let rows  = C8.split '\n' bs
        llist = map (C8.split ',') rows
        m     = fromLists (map (map (read . C8.unpack)) llist :: [[Int]])
    return m

terrain = [ (Mountain [ np (FP 98  238), np (FP 126 239)
                      , np (FP 124 260), np (FP 131 267)
                      , np (FP 123 276), np (FP 110 280)
                      , np (FP 90  278), np (FP 81  272)
                      , np (FP 95 253)])
          , (Mountain [ np (FP 106 292), np (FP 125 287)
                      , np (FP 145 270), np (FP 188 255)
                      , np (FP 190 275), np (FP 172 290)
                      , np (FP 161 313), np (FP 156 350)
                      , np (FP 139 269), np (FP 127 365)
                      , np (FP 118 350), np (FP 123 329)
                      , np (FP 131 319), np (FP 126 310)
                      , np (FP 114 308), np (FP 106 303)])
          , (Mountain [ np (FP 54 254), np (FP 70 253)
                      , np (FP 72 264), np (FP 65 271)
                      , np (FP 58 271), np (FP 59 265)
                      , np (FP 52 262)])
          , (Mountain [ np (FP 0 494), np (FP 0 449)
                      , np (FP 46  427), np (FP 68 426)
                      , np (FP 65  440), np (FP 102 447)
                      , np (FP 114 432), np (FP 136 437)
                      , np (FP 139 457), np (FP 157 461)
                      , np (FP 168 452), np (FP 231 468)
                      , np (FP 269 464), np (FP 282 446)
                      , np (FP 282 420), np (FP 299 394)
                      , np (FP 314 390), np (FP 316 422)
                      , np (FP 303 435), np (FP 311 451)
                      , np (FP 343 446), np (FP 349 435)
                      , np (FP 343 424), np (FP 352 413)
                      , np (FP 357 418), np (FP 374 413)
                      , np (FP 382 381), np (FP 364 358)
                      , np (FP 375 350), np (FP 402 353)
                      , np (FP 425 367), np (FP 436 359)
                      , np (FP 457 375), np (FP 468 379)
                      , np (FP 477 377), np (FP 476 365)
                      , np (FP 494 349), np (FP 494 467)
                      , np (FP 475 475), np (FP 453 494)
                      , np (FP 172 494), np (FP 137 475)
                      , np (FP 94  481), np (FP 83 480)
                      , np (FP 47  489), np (FP 40 494)])
          , (Mountain [ np (FP 200 237), np (FP 208 219)
                      , np (FP 229 219), np (FP 239 211)
                      , np (FP 265 203), np (FP 272 205)
                      , np (FP 293 201), np (FP 305 207)
                      , np (FP 300 225), np (FP 280 226)
                      , np (FP 244 240)])
          , (Mountain [ np (FP 209 258), np (FP 264 257)
                      , np (FP 290 276), np (FP 315 277)
                      , np (FP 325 286), np (FP 319 297)
                      , np (FP 292 299), np (FP 284 304)
                      , np (FP 265 303), np (FP 261 295)
                      , np (FP 235 297), np (FP 226 291)
                      , np (FP 227 279), np (FP 208 274)])
          , (Mountain [ np (FP 130 200), np (FP 175 174)
                      , np (FP 212 141), np (FP 231 143)
                      , np (FP 241 159), np (FP 250 162)
                      , np (FP 256 179), np (FP 250 181)
                      , np (FP 242 177), np (FP 230 185)
                      , np (FP 233 193), np (FP 230 206)
                      , np (FP 211 207), np (FP 192 215)
                      , np (FP 170 236), np (FP 163 241)
                      , np (FP 159 246), np (FP 151 245)
                      , np (FP 152 235), np (FP 136 229)])
          , (Mountain [ np (FP 42 299), np (FP 55 286)
                      , np (FP 83 286), np (FP 88 301)
                      , np (FP 102 313), np (FP 107 335)
                      , np (FP 92 336), np (FP 87 327)
                      , np (FP 42 314)])
          , (Mountain [ np (FP 0 332), np (FP 10 333)
                      , np (FP 35 309), np (FP 35 323)
                      , np (FP 73 326), np (FP 74 332)
                      , np (FP 53 339), np (FP 44 396)
                      , np (FP 40 396), np (FP 33 353)
                      , np (FP 0 374)])
          , (Mountain [ np (FP 126 380), np (FP 151 384)
                      , np (FP 158 412), np (FP 146 423)
                      , np (FP 115 415), np (FP 110 405)])
          ]

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
