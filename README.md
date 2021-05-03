# Modified ASCVD Risk Calculator
This code is for calculating the ASCVD risk at any given time based on <a href='https://www.ahajournals.org/doi/full/10.1161/01.cir.0000437741.48606.98'> the PCE paper <a> and <a href='https://www.ahajournals.org/doi/10.1161/circulationaha.107.699579'> the Framingham paper <a>. 
  
This code is modified based on <a href= 'https://github.com/syadlowsky/revised-pooled-ascvd/blob/master/original_model.R'> Syalowsky's revised-pooled-ascvd code </a>

- In the following code, the only thing we used from the PCE and Framingham papers is their coefficient estimates for the predictors. All the other values are computed from a given data
- Current code can compute the ascvd risk at an arbitrary time.
