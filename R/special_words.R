# ==========================================================
# == LISTS OF SPECIAL WORDS
# ==========================================================

question_words <- c("who", "what", "where", "when", "why", "how", "which", "whose", "whom")

# ==> START LLM src=https://aichat.uva.nl/share/97cKFYqfOX9nzVGnXn7KIvkSgsmwaLZgyenr
offensive_words <- c(
    "fuck", "fucking", "fucked", "fucker", "fuckface", "fuckhead", "fuckwit", "fuckup", "shit", "shitty", "bullshit", "shithead", "shitface", "shitbag", "shitbrain", "crap", "crappy", "craptastic",
    "asshole", "asshat", "asswipe", "assclown", "assface", "assbag", "ass", "dumbass", "jackass", "badass", "bitch", "bitchy", "bastard", "douche", "douchebag", "dick", "dickhead", "dickwad",
    "dickweed", "dickface", "prick", "prickhead", "cock", "cocksucker", "cockhead", "cockwomble", "cunt", "twat", "twatwaffle", "wanker",
    "wank", "wankstain", "tosser", "knob", "knobhead", "bellend", "bollocks", "arse", "arsehole", "arsewipe", "piss", "pissed", "pisshead", "pissbag", "pisspoor", "dammit", "damn", "goddamn",
    "hell", "jerk", "jerkoff", "loser", "louse", "creep", "creeper", "weirdo", "freak", "freakish", "idiot", "idiotic", "imbecile",
    "moron", "moronic", "dimwit", "nitwit", "numpty", "dunce", "dolt", "fool", "foolish", "buffoon", "clown", "bozo", "doofus", "dork", "nerd", "geek", "airhead", "bonehead", "blockhead", "birdbrain",
    "meathead", "knucklehead", "lamebrain", "scatterbrain", "halfwit", "simpleton", "ignoramus", "nincompoop", "numbskull", "thickhead", "thick", "stupid", "stupidest", "dumb", "dumbest", "brainless",
    "witless", "clueless", "hopeless", "pathetic", "worthless", "useless", "incompetent", "inept", "obnoxious", "annoying", "irritating", "vile",
    "disgusting", "gross", "nasty", "revolting", "repulsive", "despicable", "contemptible", "deplorable", "abhorrent", "loathsome", "odious", "whiner", "whinge", "killjoy", "blowhard",
    "hateful", "spiteful", "malicious", "cruel", "coward", "cowardly", "liar", "lying", "fraud", "phony", "fake", "hypocrite", "traitor", "snake", "rat", "scumbag", "sleazebag", "sleazeball", "slimeball",
    "lowlife", "dirtbag", "trash", "trashy", "garbage", "rubbish", "vermin", "parasite", "leech", "mooch", "deadbeat", "cheapskate", "pig", "swine", "dog", "mutt", "donkey", "mule", "beast", "monster",
    "psycho", "psychopath", "maniac", "lunatic", "nutcase", "sicko", "pervert", "degenerate", "deviant", "scoundrel", "villain", "thug", "brute", "bully", "tyrant", "bigmouth", "loudmouth", "crybaby"
)
# ==> END LLM

# ==> START LLM src=https://aichat.uva.nl/share/97cKFYqfOX9nzVGnXn7KIvkSgsmwaLZgyenr
afinn <- read.delim(
  file = paste0(
    "https://raw.githubusercontent.com/fnielsen/afinn/master/",
    "afinn/data/AFINN-111.txt"
  ),
  header = FALSE,
  sep = "\t",
  col.names = c("word", "afinn_score"),
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)
# ==> END LLM

negation_words <- c("not", "never", "no", "neither", "nor", "hardly", "barely", "scarcely")
