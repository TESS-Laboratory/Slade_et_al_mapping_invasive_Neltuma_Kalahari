S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,B1_Grid_SF,"mode")
names(S2satellite_Predict_Extract) <- c('mode')
S2satellite_Predict_Extract_DF <-bind_cols(B1_Grid_SF,S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,B1_Grid_SF,"majority" )
names(S2satellite_Predict_Extract) <- c('majority')
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, majority = S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,B1_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, S2satellite_Predict_Extract)

B1_full_Extract_DF <- S2satellite_Predict_Extract_DF

B1_full_Extract_DF[is.na(B1_full_Extract_DF)] <- 0

Drone_extract_prosopis_cover_S2Grid <- B1_full_Extract_DF%>% dplyr::select (FID,majority,frac_1)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,B2_Grid_SF,"mode")
names(S2satellite_Predict_Extract) <- c('mode')
S2satellite_Predict_Extract_DF <-bind_cols(B2_Grid_SF,S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,B2_Grid_SF,"majority" )
names(S2satellite_Predict_Extract) <- c('majority')
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, majority = S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,B2_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, S2satellite_Predict_Extract)

B2_full_Extract_DF <- S2satellite_Predict_Extract_DF

B2_full_Extract_DF[is.na(B2_full_Extract_DF)] <- 0

Temp <- B2_full_Extract_DF%>% dplyr::select (FID,majority,frac_1)
Drone_extract_prosopis_cover_S2Grid <- rbind(Temp,Drone_extract_prosopis_cover_S2Grid)
S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,B3_Grid_SF,"mode")
names(S2satellite_Predict_Extract) <- c('mode')
S2satellite_Predict_Extract_DF <-bind_cols(B3_Grid_SF,S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,B3_Grid_SF,"majority" )
names(S2satellite_Predict_Extract) <- c('majority')
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, majority = S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,B3_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, S2satellite_Predict_Extract)

B3_full_Extract_DF <- S2satellite_Predict_Extract_DF

B3_full_Extract_DF[is.na(B3_full_Extract_DF)] <- 0

Temp <- B3_full_Extract_DF%>% dplyr::select (FID,majority,frac_1)
Drone_extract_prosopis_cover_S2Grid <- rbind(Temp,Drone_extract_prosopis_cover_S2Grid)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,S1_Grid_SF,"mode")
names(S2satellite_Predict_Extract) <- c('mode')
S2satellite_Predict_Extract_DF <-bind_cols(S1_Grid_SF,S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,S1_Grid_SF,"majority" )
names(S2satellite_Predict_Extract) <- c('majority')
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, majority = S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,S1_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, S2satellite_Predict_Extract)

S1_full_Extract_DF <- S2satellite_Predict_Extract_DF

S1_full_Extract_DF[is.na(S1_full_Extract_DF)] <- 0

Temp <- S1_full_Extract_DF%>% dplyr::select (FID,majority,frac_1)
Drone_extract_prosopis_cover_S2Grid <- rbind(Temp,Drone_extract_prosopis_cover_S2Grid)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,S2_Grid_SF,"mode")
names(S2satellite_Predict_Extract) <- c('mode')
S2satellite_Predict_Extract_DF <-bind_cols(S2_Grid_SF,S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,S2_Grid_SF,"majority" )
names(S2satellite_Predict_Extract) <- c('majority')
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, majority = S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,S2_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, S2satellite_Predict_Extract)

S2_full_Extract_DF <- S2satellite_Predict_Extract_DF

S2_full_Extract_DF[is.na(S2_full_Extract_DF)] <- 0

Temp <- S2_full_Extract_DF%>% dplyr::select (FID,majority,frac_1)
Drone_extract_prosopis_cover_S2Grid <- rbind(Temp,Drone_extract_prosopis_cover_S2Grid)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,S3_Grid_SF,"mode")
names(S2satellite_Predict_Extract) <- c('mode')
S2satellite_Predict_Extract_DF <-bind_cols(S3_Grid_SF,S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,S3_Grid_SF,"majority" )
names(S2satellite_Predict_Extract) <- c('majority')
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, majority = S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,S3_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, S2satellite_Predict_Extract)

S3_full_Extract_DF <- S2satellite_Predict_Extract_DF

S3_full_Extract_DF[is.na(S3_full_Extract_DF)] <- 0

Temp <- S3_full_Extract_DF%>% dplyr::select (FID,majority,frac_1)
Drone_extract_prosopis_cover_S2Grid <- rbind(Temp,Drone_extract_prosopis_cover_S2Grid)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,S4_Grid_SF,"mode")
names(S2satellite_Predict_Extract) <- c('mode')
S2satellite_Predict_Extract_DF <-bind_cols(S4_Grid_SF,S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,S4_Grid_SF,"majority" )
names(S2satellite_Predict_Extract) <- c('majority')
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, majority = S2satellite_Predict_Extract)

S2satellite_Predict_Extract <- exact_extract(S2satellite_Predict,S4_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
S2satellite_Predict_Extract_DF <- dplyr::mutate(S2satellite_Predict_Extract_DF, S2satellite_Predict_Extract)

S4_full_Extract_DF <- S2satellite_Predict_Extract_DF

S4_full_Extract_DF[is.na(S4_full_Extract_DF)] <- 0

Temp <- S4_full_Extract_DF%>% dplyr::select (FID,majority,frac_1)
Drone_extract_prosopis_cover_S2Grid <- rbind(Temp,Drone_extract_prosopis_cover_S2Grid)
