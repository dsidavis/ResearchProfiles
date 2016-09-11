# Institutions identifiers.

x = scopusQuery(query = "affil(Davis California)",  url = "http://api.elsevier.com/content/search/affiliation")
x2 = scopusQuery(query = "affil(Davis AND California)",  url = "http://api.elsevier.com/content/search/affiliation")
x3 = scopusQuery(query = "affil(Davis) AND affil( California)",  url = "http://api.elsevier.com/content/search/affiliation")


sapply(x, `[[`, "affiliation-name")

# Includes 
#  "Univ. of Miami North Research Office"                                                          
# "REGENTS OF THE UNIVERSITY OF CALIFORNIA"
# "Evoluiton and Ecology University of California"                                                

# Many med school...
# [111] "University of California at Davis School of Medicine"                                          
#[112] "University of California at Davis School of Medicine"                                          
#[113] "University of California at Davis School of Medicine"                                          


sapply(x, `[[`, "document-count")

ndoc = as.integer(sapply(x, `[[`, "document-count")) 
w = ndoc > 0
table(w)
# FALSE  TRUE 
#   81   130 
inst = data.frame(name = sapply(x[w], `[[`, "affiliation-name"),
                  id = gsub("AFFILIATION_ID:", "", sapply(x[w], `[[`, "dc:identifier")),
                  ndocs = ndoc[w] )


inst[order(inst$ndocs, decreasing = TRUE), ]
                                                                                              name        id  ndocs
5                                                                                         UC Davis  60014439 135457
95                                                                     UC Davis School of Medicine  60023317   8842
119                                                                    UC Davis School of Medicine  60023317   8842
35                                                                         UC Davis Medical Center  60003160   6108
3                                                                 UC Davis Center for Neuroscience  60086298   1091
37                                            University California Davis Bodega Marine Laboratory  60000000    943
34                                                                          UC Davis Cancer Center  60022586    807
79                                                              California Primate Research Center  60072478    276
27                                                      California Agricultural Experiment Station 106267333     23
94                                                   California Institute of Environmental Studies 100694387     17
118                                                  California Institute of Environmental Studies 100694387     17
36                                                                        CALIFORNIA UNIVERSITY OF 107273751     13
22                                                                       Universidad de California 100742534     12
43                                                 California Asparagus Seed and Transplants, Inc.  60095695     10
101                                            California Animal Health and Food Safety Laboratory 112663466     10
125                                            California Animal Health and Food Safety Laboratory 112663466     10
32                                                University of California at Davis Medical Center 112662731      9
75                                                          California Institute for Rural Studies 100416791      9
68                                                    California Center for Public Health Advocacy 101013484      8
100                                                                  School of Veterinary Medicine 105190533      8
124                                                                  School of Veterinary Medicine 105190533      8
80                                                        California State Department of Education 101757481      7
33                                                                  U California-Davis Medical Ctr 100848672      6
78                                                            California Dairy Research Foundation 100627495      6
89                                            University of California at Davis School of Medicine 105953224      6
96                                                             Gynecology University of California 105190944      6
113                                           University of California at Davis School of Medicine 105953224      6
120                                                            Gynecology University of California 105190944      6
2                                                                      University California Davis 112783033      5
29                                                            California Tomato Research Institute 101165708      5
97                                                                       Uni versity of California 113283951      5
121                                                                      Uni versity of California 113283951      5
26                                                           California Lighting Technology Center 112177115      4
31                                                                 University of Carolina at Davis 112107406      4
39                                                 University of California at Davis Health System 107928002      4
62                                                    California Health and Food Safety Laboratory 110240189      4
74                                                                  California Anim. Hlth. Food S. 100379081      4
81                                                             National Center for Primate Biology 108065497      4
98                                                       Cellular Biology University of California 106627050      4
106                                                         John Muir Institute of the Environment 101330610      4
122                                                      Cellular Biology University of California 106627050      4
130                                                         John Muir Institute of the Environment 101330610      4
21                                                                        Universitv of California 112494519      3
28                                                                        Uniuersity of California 115740049      3
38                                                              University of Califiornia at Davis 113141982      3
72                 University of California at San Francisco and University of California at Davis 108151205      3
88                                                                  Californa IVF Fertility Center 114378597      3
92                                   Veterinary Medical Teaching Hospital University of California 113621728      3
112                                                                 Californa IVF Fertility Center 114378597      3
116                                  Veterinary Medical Teaching Hospital University of California 113621728      3
25                                                                       University o f California 115883638      2
41                                                                   The University of Californiai 100779542      2
47                                                     California Tomato Research Institute (CTRI) 101004376      2
52                                                                         Biomechanics Consulting 100806297      2
54                                                  Northern California Veterinary Specialty Group 106955179      2
69                                            University of California at Davis School of Medicine 106793623      2
73                                                       Computer Science University of California 111555020      2
77                                                Northern California Nanofabrication Center (NC2) 110375573      2
82                                  Agricultural and Resource Economics University of California - 116391089      2
85                                                               California Inst. Food Agric. Res. 100795471      2
99                                                         REGENTS OF THE UNIVERSITY OF CALIFORNIA 114142965      2
109                                                              California Inst. Food Agric. Res. 100795471      2
123                                                        REGENTS OF THE UNIVERSITY OF CALIFORNIA 114142965      2
1                                                                                   California EPA 108445334      1
4                                                                 Universiy of California at Davis 113335746      1
6                                                                        Universsity of California 108105985      1
7                                                               Biomech. Consultants of California 101353070      1
8                                                                        Univertsity of California 115473851      1
9                                                                         Universlty of California 107889931      1
10                                                                        Unviersity of California 105844522      1
11                                                                       Univerrsity of California 116327923      1
12                                                               California Age Research Institute 101754914      1
13                                                                  California Communities Program 100971255      1
14                                                                       University 1of California 111375682      1
15                                                                  Universitv of California-Davis 114048350      1
16                                                             California State Psychological Assn 106656910      1
17                                                                        Univeisity of California 116443287      1
18                                                                         Unversity of California 109538211      1
19                                                                  California Rangeland Coalition 115409938      1
20                                                                        University oi California 113068338      1
23                                                                        Universisy of California 116747061      1
24                                                                         Universly of California 116749460      1
30                                                                        Universiry of California 117248469      1
40                                              The University of California, Davis Medical Center 105401087      1
42                                                              Ctr. Neurosci. California Regl. P. 101711193      1
44                                              Sacramento and California Dermatopathology Service 108148643      1
45                                                             Univ. California at Davis and VAHCS 101279006      1
46                                                   University of California Davis Medical Center 114167340      1
48                                                   University of California, Davis Cancer Center 113281676      1
49                                                  University of California, Davis Medical Center 115554831      1
50                                                                    GSM University of California 116114742      1
51                                               Aeronautical Engineering University of California 106623875      1
53                                                                     California State 4-H Office 115479758      1
55                                                   University of California Davis Medical Center 107213248      1
56                                                              Universidad de California en Davis 113345608      1
57                                                                                    CA 95616, SA 101282930      1
58                                              University of California, Davis School of Medicine 115442783      1
59                                                               USDA ARS University of California 113196329      1
60                                            University of California at Davis School of Medicine 106733818      1
61                                                             MSCE University of California Davis 114334730      1
63                                           University of California, Davis and University Fellow 113757286      1
64                                                    Universily of California Davis Cancer Center 113171293      1
65                                                    California Animal Health and Food Laboratory 112804551      1
66                                        California Expanded Food and Nutrition Education Program 112844684      1
67                                                  Evoluiton and Ecology University of California 116723981      1
70        California National Primate Research CenterUniversity of CaliforniaDavis 95616California 113057153      1
71                            California National Primate Research Center University of California 106064673      1
76                                                 Division of Statistics University of California 106639209      1
83                             Out-of-Print Specialist University Library University of California 105780580      1
84                  Departments of Environmental Toxicology and Nutrition University of California 114871583      1
86                                              University of California, Davis School of Medicine 109525018      1
87  NMR Facility and Biomedical Engineering Graduate GroupUniversity of CaliforniaDavis California 116124446      1
90                                             The Food Intake Laboratory University of California 105882863      1
91                        KL Maddy Equine Analytical Chemistry Laboratory University of California 117016430      1
93                     College of Agricultural and Environmental Sciences University of California 117055760      1
102                                                 University of North California at Chappel Hill 106983237      1
103                                                           Univ. of Miami North Research Office 100483017      1
104                                       Center for Comparative Medicine University of California 101545059      1
105             Microbiology and Immunology School of Veterinary Medicine University of California 114476178      1
107                            Out-of-Print Specialist University Library University of California 105780580      1
108                 Departments of Environmental Toxicology and Nutrition University of California 114871583      1
110                                             University of California, Davis School of Medicine 109525018      1
111 NMR Facility and Biomedical Engineering Graduate GroupUniversity of CaliforniaDavis California 116124446      1
114                                            The Food Intake Laboratory University of California 105882863      1
115                       KL Maddy Equine Analytical Chemistry Laboratory University of California 117016430      1
117                    College of Agricultural and Environmental Sciences University of California 117055760      1
126                                                 University of North California at Chappel Hill 106983237      1
127                                                           Univ. of Miami North Research Office 100483017      1
128                                       Center for Comparative Medicine University of California 101545059      1
129             Microbiology and Immunology School of Veterinary Medicine University of California 114476178      1



# Authors

joe = getAuthorID(authlast = "Dumit", authfirst = "joseph")

getAuthorID(AFFIL = "Davis", authlast = "Marx",  AUTHFIRST = "john", SUBJABBR = "ARTS")

mx = scopusQuery(query = "AFFIL(Davis) and authlast(Marx) and AUTHFIRST(john) and SUBJABBR(ARTS)", url = "http://api.elsevier.com/content/search/author")



joe.docIDs = getAuthorDocsIds(joe)
#   @_fa                                                      prism:url         dc:identifier
#1  true  http://api.elsevier.com/content/abstract/scopus_id/0033244314  SCOPUS_ID:0033244314
#2  true http://api.elsevier.com/content/abstract/scopus_id/84980373484 SCOPUS_ID:84980373484
#3  true  http://api.elsevier.com/content/abstract/scopus_id/2342513539  SCOPUS_ID:2342513539
#4  true http://api.elsevier.com/content/abstract/scopus_id/84877826780 SCOPUS_ID:84877826780
#5  true http://api.elsevier.com/content/abstract/scopus_id/77955940728 SCOPUS_ID:77955940728
#6  true http://api.elsevier.com/content/abstract/scopus_id/29144502417 SCOPUS_ID:29144502417
#7  true http://api.elsevier.com/content/abstract/scopus_id/84887765235 SCOPUS_ID:84887765235
#8  true http://api.elsevier.com/content/abstract/scopus_id/84901834408 SCOPUS_ID:84901834408
#9  true http://api.elsevier.com/content/abstract/scopus_id/84899148897 SCOPUS_ID:84899148897
#10 true http://api.elsevier.com/content/abstract/scopus_id/84917513989 SCOPUS_ID:84917513989
#11 true http://api.elsevier.com/content/abstract/scopus_id/84960516932 SCOPUS_ID:84960516932
#12 true http://api.elsevier.com/content/abstract/scopus_id/84885549753 SCOPUS_ID:84885549753
#13 true http://api.elsevier.com/content/abstract/scopus_id/84917356214 SCOPUS_ID:84917356214
#14 true http://api.elsevier.com/content/abstract/scopus_id/84955147628 SCOPUS_ID:84955147628
#15 true http://api.elsevier.com/content/abstract/scopus_id/33748313191 SCOPUS_ID:33748313191
#16 true http://api.elsevier.com/content/abstract/scopus_id/84880677618 SCOPUS_ID:84880677618
#17 true http://api.elsevier.com/content/abstract/scopus_id/84860763458 SCOPUS_ID:84860763458
#18 true http://api.elsevier.com/content/abstract/scopus_id/84917383258 SCOPUS_ID:84917383258
#19 true http://api.elsevier.com/content/abstract/scopus_id/34547299594 SCOPUS_ID:34547299594
#20 true http://api.elsevier.com/content/abstract/scopus_id/84878912364 SCOPUS_ID:84878912364
#21 true http://api.elsevier.com/content/abstract/scopus_id/84867345337 SCOPUS_ID:84867345337


i = getDocInfo(joe.docIDs[1,3])
names(i)
#[1] "coredata"      "authors"       "language"      "authkeywords"  "idxterms"      "subject-areas" "item"         
i$"subject-areas"

names(i$coredata)
# [1] "prism:url"             "dc:identifier"         "eid"                   "dc:title"              "prism:aggregationType" "srctype"               "citedby-count"         "prism:publicationName" "source-id"             "prism:issn"           
#[11] "prism:volume"          "prism:issueIdentifier" "prism:startingPage"    "prism:endingPage"      "prism:pageRange"       "prism:coverDate"       "dc:creator"            "dc:description"        "intid"                 "link"                 

length(i$item[[2]][[3]][[1]][[2]])
# This is the bibliography.


joeDocs = getAuthorDocs(c("Dumit", "Joseph"))








#Serial Title API

jcgs = scopusQuery(title = "journal of computational and graphical statistics", url = "http://api.elsevier.com/content/serial/title")
jcgs.by.issn = scopusQuery(issn = "1061-8600", url = "http://api.elsevier.com/content/serial/title")

agri = scopusQuery(subj = "AGRI", oa = "all", url = "http://api.elsevier.com/content/serial/title")


# Subject Classifications
scopusQuery( url = "http://api.elsevier.com/content/subject/scopus")
