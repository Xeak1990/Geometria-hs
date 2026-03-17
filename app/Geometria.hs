{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty
import Data.Aeson (object, (.=))
import qualified Data.Text.Lazy as T
import Network.Wai (queryString)
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)

-- 1. FUNCIONES PURAS 
areaCirculo r = pi * r * r
perimetroCirculo r = 2 * pi * r

areaCuadrado l = l * l
perimetroCuadrado l = 4 * l

areaTriangulo b h = (b * h) / 2
perimetroTriangulo l1 l2 l3 = l1 + l2 + l3

-- parámetros en minúscula
areaTrapecio baseMayor baseMenor altura = ((baseMayor + baseMenor) * altura) / 2
perimetroTrapecio baseMayor baseMenor lado1 lado2 = baseMayor + baseMenor + lado1 + lado2

-- parámetros en minúscula
areaPentagono lado apotema = (5 * lado * apotema) / 2
perimetroPentagono lado = 5 * lado


-- 2. FUNCIÓN DE AYUDA
obtenerParam :: T.Text -> ActionM T.Text
obtenerParam nombre = do
    req <- request
    let query = queryString req
    let nombreBS = TE.encodeUtf8 (T.toStrict nombre)
    case lookup nombreBS query of
        Just (Just v) -> return (T.fromStrict (TE.decodeUtf8 v))
        _             -> return ""

-- 3. EL MAIN
main :: IO ()
main = scotty 3000 $ do
    get "/calcular" $ do
        figura <- obtenerParam "figura"
        case figura of
            "circulo" -> do
                r <- (readMaybe . T.unpack) <$> obtenerParam "radio" :: ActionM (Maybe Double)
                case r of
                    Just v -> json $ object ["figura" .= figura, "area" .= areaCirculo v, "perimetro" .= perimetroCirculo v]
                    _      -> json $ object ["error" .= ("Radio inválido" :: String)]
            "cuadrado" -> do
                l <- (readMaybe . T.unpack) <$> obtenerParam "lado" :: ActionM (Maybe Double)
                case l of
                    Just v -> json $ object ["figura" .= figura, "area" .= areaCuadrado v, "perimetro" .= perimetroCuadrado v]
                    _      -> json $ object ["error" .= ("Lado inválido" :: String)]
            
            "triangulo" -> do
                b  <- (readMaybe . T.unpack) <$> obtenerParam "base" :: ActionM (Maybe Double)
                h  <- (readMaybe . T.unpack) <$> obtenerParam "altura" :: ActionM (Maybe Double)
                l1 <- (readMaybe . T.unpack) <$> obtenerParam "l1" :: ActionM (Maybe Double)
                l2 <- (readMaybe . T.unpack) <$> obtenerParam "l2" :: ActionM (Maybe Double)
                l3 <- (readMaybe . T.unpack) <$> obtenerParam "l3" :: ActionM (Maybe Double)
                case (b, h, l1, l2, l3) of
                    (Just b', Just h', Just l1', Just l2', Just l3') -> 
                        json $ object ["figura" .= figura, "area" .= areaTriangulo b' h', "perimetro" .= perimetroTriangulo l1' l2' l3']
                    _ -> json $ object ["error" .= ("Datos incompletos" :: String)]

            "trapecio" -> do
                -- Ajustado para usar nombres de parámetros en minúscula
                bMayor <- (readMaybe . T.unpack) <$> obtenerParam "B" :: ActionM (Maybe Double)
                bMenor <- (readMaybe . T.unpack) <$> obtenerParam "b" :: ActionM (Maybe Double)
                h      <- (readMaybe . T.unpack) <$> obtenerParam "h" :: ActionM (Maybe Double)
                l1     <- (readMaybe . T.unpack) <$> obtenerParam "l1" :: ActionM (Maybe Double)
                l2     <- (readMaybe . T.unpack) <$> obtenerParam "l2" :: ActionM (Maybe Double)
                case (bMayor, bMenor, h, l1, l2) of
                    (Just bM, Just bm, Just h', Just l1', Just l2') -> 
                        json $ object ["figura" .= figura, "area" .= areaTrapecio bM bm h', "perimetro" .= perimetroTrapecio bM bm l1' l2']
                    _ -> json $ object ["error" .= ("Datos incompletos" :: String)]

            "pentagono" -> do
                l <- (readMaybe . T.unpack) <$> obtenerParam "lado" :: ActionM (Maybe Double)
                a <- (readMaybe . T.unpack) <$> obtenerParam "apotema" :: ActionM (Maybe Double)
                case (l, a) of
                    (Just l', Just a') -> 
                        json $ object ["figura" .= figura, "area" .= areaPentagono l' a', "perimetro" .= perimetroPentagono l']
                    _ -> json $ object ["error" .= ("Datos incompletos" :: String)]

            _ -> json $ object ["error" .= ("Figura no soportada" :: String)]