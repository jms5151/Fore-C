library(rethinking)
library(dagitty)

# Left out Region, Month, unobserved variables
# U1 [unobserved]

GA_dag <- dagitty( "dag {
ColSize -> GA
Cover -> GA
Fish -> GA
Temp -> GA
Development -> Kd490 -> GA
WaveEnergy -> GA
WaveEnergy -> ColSize
WaveEnergy -> Cover
WaveEnergy -> Fish
WaveEnergy -> Kd490
WaveEnergy -> Chla
Development -> Cover
Development -> Fish
Chla -> Fish
Chla -> Cover
Kd490 -> Cover
}")

drawdag(GA_dag)

impliedConditionalIndependencies(GA_dag)

WS_dag <- dagitty( "dag {
ColSize -> WS
Cover -> WS
Fish -> WS
HotSnaps -> WS
WinterCondition -> WS
WaveEnergy -> WS
WaveEnergy -> ColSize
WaveEnergy -> Cover
WaveEnergy -> Fish
WaveEnergy -> Kd490
WaveEnergy -> Chla
Chla -> Fish
Chla -> Cover
Kd490 -> Cover
}")

drawdag(WS_dag)

impliedConditionalIndependencies(WS_dag)
