# make_col_set() sep parameter works correctly

    Code
      print(make_col_set(col = rainbow(7), cuts = c(0:7), sep = "~"))
    Output
            col min max label
      1 #FF0000   0   1   < 1
      2 #FFDB00   1   2   1~2
      3 #49FF00   2   3   2~3
      4 #00FF92   3   4   3~4
      5 #0092FF   4   5   4~5
      6 #4900FF   5   6   5~6
      7 #FF00DB   6   7   > 6

# map_krig() gives the same outputs

    Code
      head(x$x)
    Output
      [1] 114.000 114.025 114.050 114.075 114.100 114.125

---

    Code
      tail(x$x)
    Output
      [1] 123.375 123.400 123.425 123.450 123.475 123.500

---

    Code
      head(x$y)
    Output
      [1] -35.500 -35.475 -35.450 -35.425 -35.400 -35.375

---

    Code
      tail(x$y)
    Output
      [1] -27.625 -27.600 -27.575 -27.550 -27.525 -27.500

