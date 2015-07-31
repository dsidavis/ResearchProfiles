names = list("Temple Lang",
     c("Smith", "MacKenzie"),
     c("Tomich", "Davis"),
     c("Sawyer", "Suzana"),
     c("Polonik", "Wolfgang"),
     c("Anderson", "Nicholas"), 
      c("Quinn", "Jim"),
      c("Halfmann"),
      c("Shauman"),
      c("Niemeier"),  
      c("Amenta"),
      c("Ma", "Kwan-Liu"),
      c("Joy", "Ken"),
      c("D'Souza", "Raissa"),
      c("Devanbu", "P"),
      c("Crutchfield", "J"),
      c("Tyson"),    # 6 and not all the same person.
      c("Burman"),
      c("Anderes"),
      c("Peng", "Jie"),
      c("Paul", "Debashis"),
c("Muller", "Hans"),
c("Lee", "Thomas"),  # 5
c("Houlton"),
c("Filkov"),
c("Matloff"),
c("Koehl"), # 2 not the same
c("Naik", "Prasad"),
c("Owens", "John"),  # 5 but 1st is the one I know.
c("Ludaescher"),
c("Mohapatra", "Prasant"),
c("Kellogg", "Louise"), 
c("Sumner", "D"),
c("Cameron", "C"),
c("Dumit"),
c("Block"),
c("Saito", "N"),
c("Friedlander", "M"),
"De Loera",
c("Tracy", "Craig"),
"Strohmer",
c("Bai", "Zhaojun"),
c("Biello", "Joseph"),
c("Knox", "L"),
c("Fassanacht", "C"),
c("Whitten", "D"),
"Freund",
c("Eisen", "J"),
c("Boucher", "S")
)


ids = lapply(names, scoGetAuthor)

names(ids) = sapply(names, function(x) do.call(paste, as.list(x)))

sapply(ids, length)

i = sapply(ids, function(x) length(x) > 2)
names(ids[i])