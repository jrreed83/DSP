module DSP where

    data FilterSpec = FilterSpec { samplesPerSymbol :: Int
                                 , rollOff          :: Double
                                 , nSymbols         :: Int 
                                 } deriving (Show) 

    data PulseShape = Normal | Sqrt deriving (Eq, Show)

    numSamples :: FilterSpec -> Int
    numSamples spec
         = (m * n) + 1
         where m = (samplesPerSymbol spec)
               n = (nSymbols spec)

    basic :: FilterSpec
    basic = FilterSpec 16 0.5  5

    sinc :: Double -> Double
    sinc 0.0 = 1.0
    sinc x   = (sin x) / x

    sincN :: Double -> Double
    sincN x = sinc (pi * x)

    dbl :: Int -> Double
    dbl i = fromIntegral i 

    rcosFir :: (Int, Double, Int, PulseShape) -> [Double]
    rcosFir (sps, beta, nSymbols, Normal) =
        (reverse taps) ++ [1] ++ taps
        where
            hh   = h (sps, beta)
            taps = [hh i | i <- [1..20]]

            --twoBeta = 2 * beta
            --h i | twoBeta * i == dbl(sps) = (pi/4) * sincN(1/twoBeta)
            --    | otherwise = sincN(xi) * cos(pi*beta*xi)/(1.0 -(twoBeta*xi)^2)
            --    where xi = (dbl i) / (dbl sps)
            --taps = [h i | i <- [1 .. 20]]
    rcosFir (sps, beta, nSymbols, Sqrt  ) = 
        []

    h :: (Int, Double) -> Int -> Double
    h (sps, beta) i =
        0.0