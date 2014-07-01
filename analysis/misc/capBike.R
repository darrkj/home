options(stringsAsFactors = FALSE)

dir <- "C:/Users/HP USER/Desktop/bikes"

files <- list.files(dir)

bikes <- NULL
for (i in files) {
  x <- paste(dir, i, sep = "/")
  bikes <- read.csv(x)
  print(names(bikes))
}

x <- paste(dir, files[8], sep = "/")
bikes <- read.csv(x)
library(lubridate)

bikes$Start.date <- parse_date_time(bikes$Start.date, "%m%d%y%H%M",
                                    tz = "UTC")
bikes$End.date <- parse_date_time(bikes$End.date, "%m%d%y%H%M",
                                  tz = "UTC")






# Suggested future sites
First Round

1  18th Street and Wyoming Avenue	NW
2	11th Street and M Street	NW
3	14th Street and Clifton Street/ Boys and Girls Club	NW
4	15th Street and Euclid Street	NW
5	20th Street and Virginia Avenue	NW
6	Ellington Bridge, SE corner	NW
7	Elm Street and 2nd Street (LeDroit Park)	NW
8	New Jersey Avenue and R Street	NW
9	Hiatt Place between Park and Irving	NW
10	13th Street and U Street	NW
11	17th Street and Massachusetts Avenue/JHU	NW
12	5th Street and Massachusetts Avenue	NW
13	8th Street and D Street	NW
14	11th Street and Florida Avenue	NW
15	11th Street and K Street	NW
16	L’Enfant Plaza at Independence Ave	SW
17	11th Street and F Street	NW
18	23rd Street and W.H.O.	NW
19	Constitution Ave and 21st Street	NW
20	34th Street and Water Street	NW
21	Connecticut and Nebraska Avenues	NW
22	Connecticut Ave and Albemarle St	NW
23	O Street and Wisconsin Ave (east)	NW
24	Wisconsin Ave and Fessenden St	NW
25	Wisconsin Ave and Veazy Street	NW
26	14th Street and Upshur Street	NW
27	14th Street and Colorado Avenue	NW
28	5th Street and Kennedy Street	NW
29	Georgia Ave and Decatur Street	NW
30	V Street and Rhode Island Ave at Summit Place	NE
31	2nd Street and M Street	NE
32	Hamlin Street and 7th Street	NE
33	12th Street and Irving Street	NE
34	Neal Street and Trinidad Avenue	NE
35	Rhode Island Ave Metro entrance	NE
36	18th Street and Rhode Island Ave	NE
37	8th Street and F Street	NE
38	Pennsylvania Ave and 3rd Street	SE
39	8th Street and East Capitol Street	NE
40	15th Street and East Capitol Street	NE
41	Independence and Washington/HHS	SW
42	Constitution Ave and 2nd St/DOL	NW
43	6th Street and Indiana Avenue	NW
44	New Jersey Avenue and D Street	SE
45	15th St, F St and Tennessee Ave	NE
46	9th Street and M Street	SE
47	Tingey Street and 3rd Street	SE
48	Deanwood Rec Center and Library	NE
49	Burroughs Avenue and 49th Street	NE
50	Burroughs Ave and Minnesota Ave	NE
51	Minnesota/34th Street and Ely Place	SE
52	Alabama Avenue and Stanton Road	SE
53	MLK, Jr. Ave and Alabama Ave	SE
54	MLK, Jr. Ave and Pleasant Street	SE

Next Round

55	MLK, Jr. Ave and St. E’s Gate 5	SE
56	14th Street and Fairmont Street	NW
57	18th Street and C Street	NW
58	L’Enfant Plaza at Banneker Circle	SW
59	G Street at MLK Library	NW
60	Wisconsin Ave and Ingomar Street	NW
61	Brandywine St and Wisconsin Ave	NW
62	Connecticut Ave and Porter Street	NW
63	O Street and Wisconsin Ave (west)	NW
64	Massachusetts Ave and 48th Street	NW
65	Van Buren Street and Rec Center	NW
66	Ft Totten Metro Station	NW
67	Cedar Street underpass (Takoma)	NW
68	Piney Branch Rd and Georgia Ave	NW
69	1st Street and K Street	NE
70	Rhode Island Ave and Franklin St	NE
71	18th Street and Monroe Street	NE
72	New Jersey Avenue and L Street	NW
73	Haines Point Rec Center	SW
74	2nd Street and V Street	SW
75	Burroughs and Division Avenues	NE
76	Ely Place and Ft. Dupont Ice Rink	SE
77	16th Street and Minnesota Ave	SE
78	MLK, Jr. Ave and St E’s Gate 1	SE