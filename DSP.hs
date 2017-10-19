module DSP where

    data FilterSpec = FilterSpec { samplesPerSymbol :: Int
                                 , rollOff          :: Double
                                 , nSymbols         :: Int 
                                 } deriving (Show) 

    

    sinc :: Float -> Float
    sinc 0.0 = 1.0
    sinc x   = (sin x) / x

    sincN :: Float -> Float
    sincN x = sinc (pi * x)

    raisedCosine :: FilterSpec -> Int -> Float
    h spec i =  
        if i == (sps / twoBeta) then
            (pi/4) * sincN(1/twoBeta)
        else 
            5.0
        end
        where beta    = rollOff spec
              twoBeta = 2 * beta
              sps     = samplesPerSymbol spec

--    raisedCosine :: FilterSpec -> [Float]
--    raisedCosine spec = 
--        let x0 = 1
