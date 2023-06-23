
# Get data of the RW group only.
scs_cs <- d2 |> 
  dplyr::filter(group == "community_sample") |>  # community_sample
  dplyr::select(all_of(starts_with("scs_")))

scs_cs$employment <- d2[d2$group== "community_sample", ]$employment

scs_cs <- scs_cs |> 
  dplyr::filter(employment != "Pensionata" & employment != "Pensionato") |>  # community_sample
  dplyr::select(all_of(starts_with("scs_")))

temp1 <- d2 |> 
  dplyr::filter(group == "rescue_workers")


hist(temp1$scs_13)
hist(temp1$scs_12)





cor(scs_cs) |> 
  round(2)

comm_sample[duplicated(as.list(scs_cs))]

psych::alpha(scs_cs)

scs_ts <- rowSums((scs_cs))
hist(scs_ts)

scs <- d3 |> 
  dplyr::filter(group == "community_sample") |>  # community_sample
  dplyr::select(all_of(starts_with("scs_")))

usdm::vif(scs_cs)

cor(scs_cs) |> 
  round(1)

e <- eigen(cov(scs_cs))

foo <- apply(scs_cs, 1, sd)

aa <- boxplot(foo)

which(foo > 2.7461538)












