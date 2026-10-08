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

# ==> START LLM src=https://aichat.uva.nl/share/9RNQRZ8vpqo6baoCYFmohOHAnh9QHKtaEiaT
group_words <- c(
  # religion
  "muslim", "muslims", "moslem", "moslems", "islam", "islamic", "islamist", "islamists",
  "jew", "jews", "jewish", "judaism",
  "evangelical", "evangelicals", "mormon", "mormons",
  "hindu", "hindus", "hinduism", "buddhist", "buddhists", "buddhism",
  "sikh", "sikhs", "sikhism", "jain", "jains",
  # migration status
  "immigrant", "immigrants", "immigration",
  "migrant", "migrants", "migration", "refugee", "refugees", "asylum",
  "foreigner", "foreigners", "undocumented", "stateless",
  # orientation, gender identity and gender
  "gay", "gays", "lesbian", "lesbians", "homo", "bi",
  "lgbt", "lgbtq", "lgbtqia", "queer", "queers",
  "trans",
  "woman", "women",
  # disability
  "disabled",
  "deaf",
  "autistic", "autism",
  "bipolar", "impaired"
)

hateful_words <- c(
  # religion
  "kike", "kikes", "kyke", "kykes", "yid", "yids", "heeb", "heebs", "hebe", "hebes",
  "jewboy", "jewboys", "zio", "zios",
  "muzzie", "muzzies", "muzzy", "mudslime", "mudslimes", "mudslim", "mudslims",
  "raghead", "ragheads", "towelhead", "towelheads", "cameljockey", "cameljockeys",
  "dothead", "dotheads", "islamofascist", "islamofascists", "kafir", "kafirs",
  # ethnicity and nationality
  "nigger", "niggers", "niggar", "niggars", "nigra", "nigras", "niglet", "niglets",
  "jigaboo", "jigaboos", "jiggaboo", "jiggaboos", "spearchucker", "spearchuckers",
  "tarbaby", "sambo", "sambos", "darkie", "darkies", "darky", "coon", "coons",
  "junglebunny", "junglebunnies", "porchmonkey", "porchmonkeys",
  "sandnigger", "sandniggers", "sandmonkey", "sandmonkeys", "dunecoon", "dunecoons",
  "shitskin", "shitskins", "kaffir", "kaffirs",
  "spic", "spics", "spick", "spicks", "wetback", "wetbacks", "beaner", "beaners",
  "chink", "chinks", "chinky", "gook", "gooks", "slanteye", "slanteyes",
  "zipperhead", "zipperheads", "chingchong", "chingchongs", "coolie", "coolies",
  "jap", "japs", "paki", "pakis", "oriental", "orientals",
  "wop", "wops", "dago", "dagos", "kraut", "krauts", "polack", "polacks",
  "limey", "limeys", "gypsy", "gypsies", "gyppo", "gyppos", "gypo", "gypos",
  "pikey", "pikeys", "injun", "injuns", "redskin", "redskins", "squaw", "squaws",
  "honky", "honkey", "honkies", "honkeys", "whitey", "whiteys", "whities", "cracker", "crackers",
  "negro", "negros", "negroes", "mulatto", "mulattos", "gringo", "gringos",
  # migration status
  "rapefugee", "rapefugees", "illegals", "anchorbaby", "anchorbabies",
  "invader", "invaders", "thirdworlder", "thirdworlders",
  # sexual orientation
  "fag", "fags", "faggot", "faggots", "faggy", "faggit", "faggits", "fagget", "fagots",
  "dyke", "dykes", "homo", "homos", "lesbo", "lesbos", "lezzie", "lezzies", "lez", "lezbo", "lezbos",
  "sodomite", "sodomites", "poof", "poofs", "poofter", "poofters",
  "fudgepacker", "fudgepackers", "carpetmuncher", "carpetmunchers",
  "battyboy", "battyboys", "buttpirate", "buttpirates",
  # gender identity
  "tranny", "trannies", "trannie", "trannys", "troon", "troons",
  "shemale", "shemales", "heshe", "heshes", "ladyboy", "ladyboys",
  # gender
  "femoid", "femoids", "foid", "foids", "feminazi", "feminazis",
  "roastie", "roasties", "mangina", "manginas",
  # disability
  "retard", "retards", "retarded", "tard", "tards", "tardo", "tardos",
  "spastic", "spastics", "spaz", "spazz", "spazzes", "spazzy",
  "cripple", "cripples", "crippled", "gimp", "gimps",
  "mong", "mongs", "mongoloid", "mongoloids", "windowlicker", "windowlickers",
  "autist", "autists", "sperg", "spergs", "sperglord", "sperglords"
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
  encoding = "UTF-8",
  quote = ""
)
# ==> END LLM

negation_words <- c("not", "never", "no", "neither", "nor", "hardly", "barely", "scarcely")
