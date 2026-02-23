##### Script resum per filtrar i visualitzar els resultats finals

punts_finals <- st_read("results/07_Llegir_punts_LlistaVermella/Punts_finals.gpkg")

unique(punts_finals$Sel_HIC)
unique(punts_finals$Sel_LV)
unique(punts_finals$Sel_GrupCORINE)


## Per HIC

punts_finals |> group_by(COD_HIC, RegioHIC) |> 
    summarise(N = n()) |> 
    arrange(N)

hic_inf_30 <- punts_finals |> group_by(COD_HIC, RegioHIC) |> 
    summarise(N = n()) |> 
    filter(N < 30) 

## Per Grup CORINE

grup_inf_30 <- punts_finals |> group_by(Codi_grup) |> 
    summarise(N = n()) |> 
    filter(N < 30) 

## Per CORINE Llista Vermella

corine_lv_inf_30 <- punts_finals |>
    filter(Llista_ver == "LV") |> 
    group_by(COD_CORINE) |> 
    summarise(N = n()) |> 
    filter(N < 30) 

punts_finals |> 
    filter(Sel_HIC==1 & Sel_GrupCORINE == 1 & Sel_LV == 1) |> 
    nrow()

punts_finals |> 
    filter(Sel_HIC==1 & Sel_GrupCORINE == 1) |> 
    nrow()

nrow(punts_finals)

boscos |> filter(Codi_grup=="GQrFa")



st_write(punts_finals, "results/08_Punts_finals/Punts_mostreig_ECH.gpkg")
