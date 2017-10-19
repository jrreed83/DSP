module DSP where

    data FilterSpec = FilterSpec { samplesPerSymbol :: Int
                                 , rollOff          :: Double
                                 , nSymbols         :: Int 
                                 } deriving (Show) 

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

    raisedCosine :: FilterSpec -> Int -> Double
    raisedCosine spec i =  
        if abs(iD) == ( spsD / twoBeta) then
            (pi/4) * sincN(1/twoBeta)
        else 
            sincN(xi) * cos(pi*beta*xi)/(1.0 -(2.0*beta*xi)^2)
        where beta    = rollOff spec
              twoBeta = 2 * beta
              sps     = samplesPerSymbol spec
              iD      = (fromIntegral i)
              spsD    = (fromIntegral sps)
              xi      = iD/spsD
    
    design :: FilterSpec -> [Double]
    design spec = 
        [h i | i <- [-5..5]]
        where h = raisedCosine spec
--    raisedCosine :: FilterSpec -> [Float]
--    raisedCosine spec = 
--        let x0 = 1
