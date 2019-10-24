
rock.table <- read.table(textConnection(
  'id name
  1 "sedimentary"
  2 "igneous"
  3 "covered"
  '), header=TRUE, stringsAsFactors=FALSE
)

litho.table <- read.table(textConnection(
  'id name def_col
  1 "claystone" "#DDCBBF"
  2 "siltstone" "#CECCAC"
  3 "mudstone" "#CACACA"
  4 "shale" "gray50"
  5 "sandstone" "#ECEE00"
  6 "conglomerate" "goldenrod1"
  7 "breccia" "gold3"
  8 "limestone" "skyblue1"
  9 "dolomite" "royalblue1"
  10 "bentonite" "white"
  11 "chalk" "lightblue3"
  12 "chert" "khaki3"
  13 "coal" "black"
  14 "diatomite" "beige"
  15 "glauconite" "white"
  16 "gypsum" "orchid1"
  17 "halite" "paleturquoise1"
  18 "limonite" "white"
  19 "phosphorite" "sandybrown"
  20 "siderite" "tan"
  21 "agglomerate" "grey30"
  22 "pyroclastic breccia" "peachpuff2"
  23 "lapillistone" "grey"
  24 "tuff" "grey"
  25 "igneous rock" "palevioletred4"
  26 "granite" "palevioletred4"
  27 "basalt" "palevioletred4"
  28 "metamorphic rock" "white"
  29 "slate" "white"
  30 "schist" "white"
  31 "gneiss" "white"
  32 "serpentinite" "white"
  33 "quartzite" "white"
  '), header=TRUE, stringsAsFactors=FALSE
)

gs.table <- read.table(textConnection(
  'id name xAxis
1 "clay" 0.19
2 "clay / silt" 0.26
3 "silt" 0.33
4 "silt / very fine sand" 0.36
5 "very fine sand" 0.39
6 "very fine / fine sand" 0.42
7 "fine sand" 0.45
8 "fine / medium sand" 0.48
9 "medium sand" 0.51
10 "medium / coarse sand" 0.54
11 "coarse sand" 0.57
12 "coarse / very coarse sand" 0.6
13 "very coarse sand" 0.63
14 "very coarse / granule" 0.665
15 "granule" 0.7
16 "granule / pebble" 0.735
17 "pebble" 0.77
18 "pebble / cobble" 0.805
19 "cobble" 0.84
20 "cobble / boulder" 0.875
21 "boulder" 0.91
22 "mudstone" 0.22
23 "wackestone" 0.33
24 "packstone" 0.63
25 "grainstone" 0.77
26 "boundstone" 0.91
27 "floatstone" 0.91
28 "rudstone" 0.91
29 "bafflestone" 0.91
30 "bindstone" 0.91
31 "framestone" 0.91
32 "crystalline" 0.96
33 "fine ash" 0.33
34 "medium ash" 0.51
35 "coarse ash" 0.63
36 "fine lapilli" 0.7
37 "medium lapilli" 0.77
38 "coarse lapilli" 0.77
39 "fine block" 0.84
40 "coarse block" 0.91
'), header=TRUE, stringsAsFactors=FALSE
)

stain.table <- read.table(textConnection(
  'id name color
  1 "weak" "grey100"
  2 "moderate weak" "grey75"
  3 "moderate" "grey50"
  4 "moderate strong" "grey25"
  5 "strong" "grey0"
  '), header=TRUE, stringsAsFactors=FALSE
)

biotur.table <- read.table(textConnection(
  'id name percent
  1 "sparse" 0.166
  2 "low" 0.333
  3 "moderate" 0.5
  4 "high" 0.666
  5 "intense" 0.83
  6 "complete" 1
  '), header=TRUE, stringsAsFactors=FALSE
)

fossil.table <- read.table(textConnection(
  'id name file_name
  1 "macrofossils" "f_s1"
  2 "invertebrates" "f_s2"
  3 "annelids" "f_s3"
  4 "arthropods" "f_s4"
  5 "arachnids" "f_s5"
  6 "crustaceans" "f_s6"
  7 "insects" "f_s7"
  8 "trilobites" "f_s8"
  9 "brachiopods" "f_s9"
  10 "bryozoans" "f_s10"
  11 "cnidarians" "f_s11"
  12 "corals" "f_s12"
  13 "stromatoporoids" "f_s13"
  14 "echinoderms" "f_s14"
  15 "crinoids" "f_s15"
  16 "echinoids" "f_s16"
  17 "graptolites" "f_s17"
  18 "mollusks" "f_s18"
  19 "cephalopods" "f_s19"
  20 "ammonoids" "f_s20"
  21 "belemnoids" "f_s21"
  22 "nautiloids" "f_s22"
  23 "gastropods" "f_s23"
  24 "pelecypods" "f_s24"
  25 "sponges" "f_s25"
  26 "vertebrates" "f_s26"
  27 "amphibians" "f_s27"
  28 "fish" "f_s28"
  29 "mammals" "f_s29"
  30 "reptiles" "f_s30"
  31 "plants" "f_s32"
  32 "leaves" "f_s32"
  33 "roots" "f_s33"
  34 "wood" "f_s34"
  35 "algae" "f_s35"
  36 "conifers" "f_s36"
  37 "ferns" "f_s37"
  38 "flowering plants" "f_s38"
  39 "stromatolites" "f_s39"
  40 "fungi" "f_s40"
  41 "trace fossils" "f_s41"
  42 "burrows" "f_s42"
  43 "coprolites" "f_s43"
  44 "tracks" "f_s44"
  45 "microfossils" "f_s45"
  46 "conodonts" "f_s46"
  47 "diatoms" "f_s47"
  48 "foraminifera" "f_s48"
  49 "larger foraminifera" "f_s49"
  50 "benthonic foraminifera" "f_s50"
  51 "planktonic foraminifera" "f_s51"
  52 "nannofossils" "f_s52"
  53 "ostracodes" "f_s53"
  54 "palynomorphs" "f_s54"
  55 "acritarchs" "f_s55"
  56 "chitinozoans" "f_s56"
  57 "dinoflagellates" "f_s57"
  58 "pollen - spores" "f_s58"
  59 "radiolarians" "f_s59"
  60 "silicoflagellates" "f_s60"
  61 "spicules" "f_s61"
  '), header=TRUE, stringsAsFactors=FALSE
)

tracefossil.table <- read.table(textConnection(
  'id name file_name
1 "arenicolites" "tf_s1"
2 "asteriacites" "tf_s2"
3 "caulostrepsis" "tf_s3"
4 "chondrites" "tf_s4"
5 "cosmorhaphe" "tf_s5"
6 "cruziana" "tf_s6"
7 "diplocraterion" "tf_s7"
8 "echinoid borings" "tf_s8"
9 "entobia" "tf_s9"
10 "gastrochaenolites" "tf_s10"
11 "gyrochorte" "tf_s11"
12 "helminthoida" "tf_s12"
13 "lorenzinia" "tf_s13"
14 "macanopsis" "tf_s14"
15 "muenesteria" "tf_s15"
16 "nereites" "tf_s16"
17 "ophiomorpha" "tf_s17"
18 "paleodictyon" "tf_s18"
19 "phycodes" "tf_s19"
20 "planolites" "tf_s20"
21 "psilonichnus" "tf_s21"
22 "rhizocorallium" "tf_s22"
23 "skolithos" "tf_s23"
24 "spirorhaphe" "tf_s24"
25 "taphrehelminthopsis" "tf_s25"
26 "teichichnus" "tf_s26"
27 "teredolites" "tf_s27"
28 "thalassinoides" "tf_s28"
29 "trypanites" "tf_s29"
30 "zoophycos" "tf_30"
31 "vertebrate footprints" "tf_s31"
'), header=TRUE, stringsAsFactors=FALSE
)

others.table <- read.table(textConnection(
  'id name file_name
1 "concretions" "other_s1"
2 "nodules" "other_s2"
3 "intraclasts" "other_s3"
4 "calcite" "other_s4"
5 "feldspar" "other_s5"
6 "glauconite" "other_s6"
7 "gypsum" "other_s7"
8 "kaolinite" "other_s8"
9 "mica" "other_s9"
10 "olivine" "other_s10"
11 "pyrite" "other_s11"
12 "pyroxene" "other_s12"
13 "siderita" "other_s13"
14 "sulphur" "other_s14"
15 "undifferentiated organic matter" "other_s15"
'), header=TRUE, stringsAsFactors=FALSE
)

sed.struc.table <- read.table(textConnection(
'id name file_name
1 "massive" "sd_s1"
2 "planar lamination" "sd_s2"
3 "lenticular lamination" "sd_s3"
4 "wavy lamination" "sd_s4"
5 "irregular wavy lamination" "sd_s5"
6 "flaser lamination" "sd_s6"
7 "ripple lamination" "sd_s7"
8 "cross-lamination" "sd_s8"
9 "planar cross-stratification" "sd_s9"
10 "through cross-stratification" "sd_s10"
11 "hummocky cross-stratification" "sd_s11"
12 "swaley cross-stratification" "sd_s12"
13 "herringbone cross-stratification" "sd_s13"
14 "flute casts" "sd_s14"
15 "horizontal bedding" "sd_s15"
16 "normal grading" "sd_s16"
17 "inverse grading" "sd_s17"
31 "slump" "sd_s31"
32 "convolute lamination" "sd_s32"
33 "flame" "sd_s33"
34 "dish and pillar" "sd_s34"
35 "load casts" "sd_s35"
36 "mudcracks" "sd_s36"
37 "syneresis cracks" "sd_s37"
38 "cone in cone" "sd_s38"
39 "stylolites" "sd_s39"
40 "pseudo nodules" "sd_s40"
'), header=TRUE, stringsAsFactors=FALSE
)

samples.table <- read.table(textConnection(
  'id name pch color
  1 "rock sample" 15 "chocolate4"
  2 "fossil sample" 16 "springgreen4"
  3 "foraminifera" 17 "deepskyblue3"
  4 "geochemistry" 18 "slateblue3"
  5 "palinology" 8  "darkblue"
  6 "petrography" 9 "tomato3"
  7 "petrophysics" 7 "black"
  8 "XRD" 11 "orangered1"
  '), header=TRUE, stringsAsFactors=FALSE
)

width.table <- read.table(textConnection(
  'id name arg value
  1 "w.oma" "w.oma" 1.5
  2 "w.int" "w.int" 0.5
  3 "w.lithostrat" "d.lithostrat" 0.75
  4 "w.ncore" "d.ncore" 0.75
  5 "w.oil.stain" "d.oil.stain" .55
  6 "w.bioturbation" "d.bioturbation" 2
  7 "w.barscale" "d.graphic_bar" 1.25
  8 "w.GR.log" "d.GR.log" 3.25
  9 "w.fossils" "d.fossils" 1.5
  10 "w.tracefossils" "d.tracefossils" 1.5
  11 "w.sed.structures" "d.sed.structures" 1.5
  12 "w.other.sym" "d.other.sym" 1.5
  13 "w.struct.data" "d.struct.data" 1
  14 "w.bed.number" "d.bed.number" 0.75
  15 "w.lithology" "d.lithology" 5.93
  16 "w.samples" "d.samples" 1.75
  '), header=TRUE, stringsAsFactors=FALSE
)

litho.unit.table <- read.table(textConnection(
  'id name
  1 "group"
  2 "formation"
  3 "member"
  4 "informal unit"
  5 "informal member"
  '), header=TRUE, stringsAsFactors=FALSE
)

headerLines <- read.table(textConnection(
  'x0 x1 y0 y1 lwd colour
  0 0.91 0 0 1.1 "red"
  0 0.91 0.37 0.37 0.8 "black"
  0 0.91 0.54 0.54 0.8 "black"
  0 0.91 0.71 0.71 0.8 "black"
  0 0 -0.15 1 1.1 "black"
  0.19 0.19 -0.02 0.03 0.5 "red"
  0.33 0.33 -0.12 0.03 0.9 "red"
  0.39 0.39 -0.02 0.04 0.5 "red"
  0.45 0.45 -0.02 0.05 0.5 "red"
  0.51 0.51 -0.02 0.06 0.5 "red"
  0.57 0.57 -0.02 0.07 0.5 "red"
  0.63 0.63 -0.12 0.08 0.9 "red"
  0.70 0.70 -0.02 0.09 0.5 "red"
  0.77 0.77 -0.02 0.10 0.5 "red"
  0.84 0.84 -0.02 0.11 0.5 "red"
  0.33 0.33 0.37 0.71 0.9 "red"
  0.63 0.63 0.37 0.92 0.9 "red"
  0.19 0.19 0.54 0.92 0.5 "red"
  0.77 0.77 0.54 0.92 0.5 "red"
  '), header=TRUE, stringsAsFactors=FALSE
)

headerText <- read.table(textConnection(
  'label x y just rot cex font
  "Mudstone" 0.16 0.45 "center" 0 1 2
  "Sandstone" 0.48 0.45 "center" 0 1 2
  "Conglomerate" 0.805 0.45 "center" 0 0.9 2
  "0.004" 0.19 0.05 "left" 90 0.9 1
  "0.062" 0.33 0.05 "left" 90 0.9 1
  "0.125" 0.39 0.06 "left" 90 0.9 1
  "0.25" 0.45 0.07 "left" 90 0.9 1
  "0.5" 0.51 0.08 "left" 90 0.9 1
  "1" 0.57 0.09 "left" 90 0.9 1
  "2" 0.63 0.10 "left" 90 0.9 1
  "4" 0.70 0.11 "left" 90 0.9 1
  "64" 0.77 0.12 "left" 90 0.9 1
  "256" 0.84 0.13 "left" 90 0.9 1
  "clay" 0.09 -0.049 "left" 0 0.9 1
  "silt" 0.25 -0.049 "left" 0 0.9 1
  "vf" 0.34 -0.049 "left" 0 0.9 1
  "f" 0.425 -0.049 "left" 0 0.9 1
  "m" 0.465 -0.049 "left" 0 0.9 1
  "c" 0.54 -0.049 "left" 0 0.9 1
  "vc" 0.58 -0.049 "left" 0 0.9 1
  "gr" 0.66 -0.049 "left" 0 0.9 1
  "pe" 0.73 -0.049 "left" 0 0.9 1
  "co" 0.80 -0.049 "left" 0 0.9 1
  "bo" 0.87 -0.049 "left" 0 0.9 1
  "Mdst" 0.105 0.62 "center" 0 0.9 1
  "Wkst" 0.26 0.62 "center" 0 0.9 1
  "Pkst" 0.49 0.62 "center" 0 0.9 1
  "Grst" 0.70 0.62 "center" 0 0.9 1
  "Boundst" 0.775 0.62 "left" 0 0.9 1
  "Fine" 0.105 0.87 "center" 0 1 1
  "tuff" 0.105 0.78 "center" 0 1 1
  "Coarse tuff" 0.42 0.82 "center" 0 1 1
  "Lapill" 0.635 0.87 "left" 0 0.9 1
  "stone" 0.635 0.78 "left" 0 0.9 1
  "Pyroclast" 0.775 0.87 "left" 0 1 1
  "breccia" 0.775 0.78 "left" 0 1 1
  '), header=TRUE, stringsAsFactors=FALSE
)
