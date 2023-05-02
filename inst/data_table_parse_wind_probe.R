r[, c("wind_probe_3.m", "wind_probe_10.m") :=
    transpose(wind_probe_height.m)]
r[, wind_probe_3.m := fifelse(wind_probe_3.m == 3, TRUE, FALSE)]
r[, wind_probe_10.m := fifelse(wind_probe_10.m == 10, TRUE, FALSE)]
r[, wind_probe_3.m := fifelse(is.na(wind_probe_3.m), FALSE,
                              wind_probe_3.m)]
r[, wind_probe_10.m := fifelse(is.na(wind_probe_10.m), FALSE,
                               wind_probe_10.m)]
