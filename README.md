# pallindrome

 This is my humble attempt at finding long pallindromes composed of English words found in a common dictionary.

 The basic approach is like Peter Norvig's that searches for the remainder of the previous pallindromic match. For example "amen .... nema" -> "amen ... cinema" --> "amen ic ... cinema" --> "amen ice ... cinema"

 With a starting word of "amen" this humble approach will find the following 1376 word pallindrome.

("amen" "ice" "tabasco" "dab" "lea" "gab" "lab" "arak" "cabal" "label" "echo"
 "dad" "negate" "baa" "had" "dado" "orbit" "nab" "rabat" "road" "image" "bad"
 "ocelot" "sac" "rice" "bade" "sumatra" "pace" "sad" "nape" "lebanon" "aaron"
 "esau" "qatar" "araks" "abut" "sag" "lacs" "idaho" "lad" "nadir" "adam" "one"
 "boreal" "aromas" "dday" "rial" "anally" "dial" "aden" "obese" "egg" "nomadic"
 "ulster" "ebb" "iberia" "zag" "gadded" "debar" "bag" "nodal" "obey" "adios"
 "unisex" "adit" "oracle" "gnash" "table" "bah" "placid" "ogres" "redial"
 "inane" "mac" "rad" "exam" "ado" "cab" "mama" "eras" "diana" "bail" "icecap"
 "aesop" "mock" "radar" "afar" "pock" "rebate" "bake" "rues" "liar" "again"
 "imago" "tacit" "ores" "miami" "dairy" "saga" "gag" "email" "gage" "moan"
 "erase" "lade" "grabs" "alanine" "lag" "naples" "madam" "rubs" "plaid" "imam"
 "octal" "bale" "gutless" "umbra" "gain" "sober" "age" "keel" "fab" "mural"
 "octet" "omen" "imams" "apse" "diaper" "pad" "oscillate" "mag" "notary" "laid"
 "nib" "albany" "maid" "emery" "lain" "amass" "off" "filch" "ago" "bali" "name"
 "lair" "epic" "oft" "relabel" "paid" "opal" "uvular" "rock" "cuban" "tea"
 "iran" "mutual" "unnamed" "ides" "sobs" "laminar" "atlas" "unary" "trammel"
 "ash" "crab" "oblate" "facia" "major" "fag" "love" "sinai" "grocer" "await"
 "fume" "donate" "rebel" "citrate" "ball" "ive" "pail" "edema" "cam" "mocker"
 "tag" "oyster" "gear" "ilk" "subdue" "fair" "amigo" "yak" "rape" "disaster"
 "refer" "oboe" "dorsal" "live" "take" "ego" "slab" "mobs" "cradle" "hebrew"
 "snag" "nibs" "trad" "edible" "wodan" "augite" "practise" "raisin" "utah"
 "siege" "vain" "arm" "uranus" "obsess" "allot" "amir" "game" "near" "ammo"
 "tale" "udder" "obsessed" "doge" "wad" "ribbon" "keen" "keep" "urea" "pupa"
 "slabs" "sage" "lam" "goddess" "ufo" "tuareg" "nag" "noble" "ear" "reissue"
 "zap" "apery" "pan" "ultimo" "van" "named" "obsesses" "sago" "can" "iterate"
 "zebu" "cat" "ledge" "pat" "arrest" "essay" "grenade" "elbe" "bat" "oil"
 "libel" "bibles" "roman" "uteri" "sedge" "vat" "lama" "cock" "rocks" "eden"
 "robe" "libels" "sahib" "ogle" "maned" "lobe" "glibly" "bison" "inlet" "naive"
 "lava" "jab" "maser" "idiot" "iotas" "ivory" "gave" "negev" "ruck" "circle"
 "efface" "dais" "repay" "biles" "pocks" "amp" "said" "away" "oslo" "cats"
 "ivy" "naomi" "lee" "dam" "meld" "nobler" "gnome" "tibia" "reset" "upside"
 "lobed" "eclair" "term" "oodles" "sated" "aver" "ruby" "retral" "warble"
 "gels" "idle" "ifs" "debit" "eye" "nice" "loch" "treble" "urge" "mocks"
 "umlaut" "never" "edict" "pate" "idler" "tepee" "fed" "old" "lochs" "idles"
 "nits" "feeble" "veld" "rococo" "leer" "fees" "self" "fuddle" "ham" "ill"
 "luck" "coder" "eels" "suture" "same" "mercy" "trade" "bucks" "ire" "perch"
 "cribs" "goblet" "nile" "idol" "open" "ids" "dubs" "tuba" "video" "debut"
 "tsetse" "recede" "cess" "docks" "ureter" "cession" "illicit" "tales" "seven"
 "odd" "rucks" "atop" "meted" "oner" "often" "nils" "loci" "he" "midday" "deer"
 "upon" "rope" "gridded" "nee" "modem" "ohm" "lees" "odder" "ream" "uproo"
 "degas" "ode" "podded" "loft" "rake" "ergo" "meddle" "we" "vodka" "oaf" "lose"
 "swords" "pooch" "tribe" "cuds" "process" "opener" "espy" "lope" "kudu"
 "medial" "niece" "ergot" "tide" "nude" "ken" "oxalate" "pecan" "ruff" "logo"
 "den" "egress" "orgy" "subs" "tocsin" "nets" "march" "toby" "began" "note"
 "tiled" "eider" "unit" "lobes" "legal" "feet" "tesseral" "felt" "tilde"
 "modes" "oompah" "charon" "emetic" "nine" "biz" "anew" "eery" "ted" "irk"
 "lemon" "eve" "poled" "need" "order" "ruffle" "sties" "rome" "fifes" "ions"
 "porch" "trifle" "tomb" "muds" "such" "sift" "subset" "odes" "ones" "rune"
 "life" "croft" "tube" "nohow" "tee" "referral" "lock" "lofts" "recite" "dies"
 "tuck" "cohort" "nines" "sea" "vole" "onset" "imp" "mulch" "selfsame" "nepal"
 "fen" "olden" "evilness" "eugenic" "roped" "urn" "robed" "islam" "redden"
 "even" "rocs" "wedded" "light" "right" "older" "unite" "lifetime" "seer" "ten"
 "impart" "nest" "ridden" "evils" "midden" "woe" "tikka" "ewes" "opposed"
 "onto" "clang" "isomer" "push" "serfs" "gods" "nodded" "rigor" "igloo" "few"
 "ok" "nip" "suck" "conker" "tildes" "semen" "onward" "revoke" "emir" "otter"
 "flab" "revel" "impi" "devil" "ole" "eros" "rote" "kill" "lidded" "rowers"
 "moods" "rewards" "yards" "murder" "olm" "regime" "sen" "noted" "insert"
 "sulk" "rule" "mild" "eyes" "less" "ems" "evener" "web" "yams" "pilfer"
 "culdesac" "nudes" "simple" "heir" "every" "sock" "ramp" "lugs" "golfer"
 "import" "serif" "tipster" "ruth" "cruller" "rose" "mimes" "opt" "rider"
 "timbre" "hem" "users" "gofer" "opts" "elan" "err" "erupt" "nods" "draught"
 "rime" "time" "pomelo" "recur" "to" "get" "avow" "oval" "slam" "mammy" "glen"
 "nuts" "plugs" "mugs" "tugs" "knit" "penile" "perk" "limb" "ups" "revels"
 "pile" "lump" "miles" "revere" "pipes" "unlike" "mill" "lords" "gold" "losers"
 "pools" "tools" "told" "lost" "in" "words" "revolt" "ell" "ludo" "heman"
 "rush" "tome" "popes" "revs" "wold" "uses" "rows" "rooms" "pomp" "morn"
 "unlit" "snip" "murk" "nuptial" "pelt" "surd" "news" "some" "yrs" "melt"
 "ruts" "wept" "lily" "struck" "owl" "lung" "nursery" "plato" "togo" "prat"
 "snap" "snare" "roo" "wrap" "sneer" "pro" "tore" "rosy" "sons" "puppet" "spot"
 "stir" "writ" "stop" "step" "pups" "nosy" "sore" "rotor" "preen" "spar" "woo"
 "reran" "span" "star" "pogo" "total" "pyres" "rung" "null" "wok" "curtsy"
 "lilt" "pews" "turtle" "ms" "rye" "moss" "wend" "rustle" "plait" "punk" "rump"
 "instil" "nun" "romp" "mops" "moors" "worse" "sud" "lows" "verse" "pope"
 "moth" "surname" "ho" "dull" "let" "lovers" "drown" "it" "sold" "lots" "loots"
 "loops" "resold" "logs" "droll" "limekiln" "use" "pipe" "reverse" "limp"
 "mule" "lips" "levers" "pub" "milk" "repel" "inept" "inks" "guts" "gums"
 "gulps" "tunnel" "gym" "mammal" "slav" "ow" "ovate" "go" "truce" "role" "mope"
 "mite" "mirth" "guards" "dont" "purer" "renal" "est" "pore" "fogs" "resume"
 "herb" "mitre" "dirt" "pose" "mime" "sorrel" "lurch" "turret" "spitfire"
 "strop" "mire" "flogs" "gulp" "mark" "cosy" "reverie" "help" "missed"
 "uncased" "lucre" "flips" "maybe" "wren" "eves" "mess" "els" "eyed" "lime"
 "lurk" "lustre" "snide" "tonne" "semi" "germ" "lore" "drums" "drays" "drawers"
 "dooms" "reworded" "dill" "like" "torso" "reel" "olive" "dip" "mile" "verbal"
 "fret" "tori" "meek" "overdraw" "none" "messed" "litre" "knock" "cusp" "ink"
 "owe" "fool" "giro" "girded" "dons" "dogs" "fresh" "supremo" "signal" "cot"
 "nodes" "oppose" "weak" "kite" "owned" "dims" "livened" "dirts" "entrap"
 "mine" "tree" "semite" "filet" "inured" "loth" "girth" "gilded" "dews" "corn"
 "evened" "dermal" "side" "born" "rude" "porcine" "guess" "enlivened" "lone"
 "flap" "enemas" "flesh" "clump" "mites" "noel" "ova" "essen" "intro" "hock"
 "cuts" "eidetic" "erst" "folk" "collar" "referee" "two" "hone" "butt" "force"
 "file" "nurse" "nose" "dotes" "bust" "fish" "cuss" "dumb" "motel" "firth"
 "crops" "noise" "fife" "morse" "itself" "furred" "rode" "end" "elope" "venom"
 "elk" "ride" "tyre" "ewe" "nazi" "ben" "incite" "menorah" "chap" "moose"
 "domed" "little" "flares" "settee" "flag" "else" "bolt" "inure" "died" "elite"
 "tonnage" "by" "both" "crams" "tennis" "cots" "busy" "grosser" "gene" "do"
 "golf" "furnace" "petal" "axon" "eke" "dune" "ditto" "greece" "inlaid" "emu"
 "duke" "polyp" "serene" "posse" "corps" "duce" "birth" "coops" "drowse"
 "solfa" "oak" "dove" "weld" "demo" "greek" "art" "folded" "dope" "dosage"
 "door" "puma" "erred" "dose" "elm" "home" "dome" "ended" "dirge" "porno"
 "puree" "dyad" "dime" "hi" "cols" "linnet" "fore" "node" "tempo" "task" "curd"
 "done" "vessel" "attic" "illinois" "secrete" "rusk" "cods" "secede" "ceres"
 "testtube" "doe" "diva" "buts" "buds" "dine" "polo" "die" "lintel" "bogs"
 "birch" "crepe" "risk" "cubed" "arty" "creme" "maseru" "tussle" "ere" "dock"
 "cull" "lima" "held" "duff" "lessee" "free" "loco" "cord" "level" "beefs"
 "tinsel" "dish" "cold" "lode" "fee" "petrel" "diet" "apt" "cider" "eventual"
 "musk" "come" "gruel" "berth" "cole" "cine" "yeti" "beds" "field" "isle" "gel"
 "brawl" "artery" "burr" "evade" "tassel" "doom" "retrial" "cede" "bole"
 "dispute" "serai" "bite" "mongrel" "bond" "lemma" "dee" "limo" "any" "vista"
 "col" "soya" "wadi" "asp" "mask" "copse" "libya" "persia" "decaf" "feel"
 "crick" "curve" "geneva" "gyro" "visa" "toitoi" "dire" "samba" "java" "levi"
 "ant" "elnino" "sibyl" "bilge" "bold" "enamel" "gobi" "hassle" "bile" "borne"
 "desk" "cork" "coca" "malta" "veg" "desire" "tuna" "morsel" "bible" "bill"
 "iota" "be" "bleed" "anergy" "assets" "errata" "peg" "delta" "cube" "zeta"
 "retina" "cog" "assesses" "bode" "manna" "vomit" "luna" "pyre" "papa" "zeus"
 "sierra" "eel" "bong" "anger" "auto" "fussed" "dogma" "leg" "ass" "balsa"
 "pupae" "rupee" "knee" "knob" "bird" "awe" "goddesses" "bored" "duel" "atom"
 "mara" "enema" "grim" "atoll" "asses" "bosun" "arum" "rani" "ave" "geisha"
 "tunisia" "resit" "carpet" "iguana" "dowel" "bided" "arts" "bing" "answer"
 "beheld" "arcs" "bomb" "also" "geek" "ate" "villas" "rodeo" "bore" "ferrets"
 "aside" "parka" "yogi" "maria" "feud" "busk" "lira" "egrets" "yoga" "trek"
 "comma" "came" "deli" "ape" "villa" "bet" "article" "beret" "anode" "mufti"
 "aware" "corgi" "anise" "volga" "fro" "jamaica" "fetal" "bob" "arch" "salem"
 "martyr" "anus" "altar" "animals" "bossed" "idem" "annul" "autumn" "aria"
 "etna" "buck" "corral" "uvula" "podia" "pleb" "alert" "foci" "peri" "ale"
 "manila" "bog" "ah" "cliff" "fossa" "mania" "lyre" "media" "myna" "blab"
 "india" "lyra" "tonga" "metallic" "soda" "prep" "aide" "spasm" "amine" "motet"
 "cola" "rumba" "flee" "keg" "are" "bosnia" "garb" "mussel" "tugela" "blat"
 "coma" "midi" "alps" "burma" "damsel" "panga" "lenin" "alas" "barged" "ales"
 "arena" "omega" "glia" "mega" "gaga" "syria" "dim" "aims" "erotica" "toga"
 "mini" "agar" "ails" "eureka" "beta" "berk" "copra" "farad" "ark" "compose"
 "apace" "cilia" "ban" "aids" "area" "mamba" "cod" "am" "axed" "arc" "amen"
 "anil" "aiders" "ergodic" "alpha" "bel" "baths" "angel" "carotid" "axe"
 "sinusoid" "aye" "bola" "donga" "bra" "bedded" "dagga" "zaire" "bib" "berets"
 "lucid" "among" "geese" "boned" "ala" "idyll" "anal" "airy" "adds" "amoral"
 "aerobe" "nomad" "arid" "and" "aloha" "disc" "alga" "stub" "ask" "ararat"
 "aqua" "senora" "anon" "abele" "panda" "sec" "apart" "amused" "abe" "circa"
 "stole" "coda" "beg" "amid" "aorta" "barb" "anti" "brood" "add" "aha" "abet"
 "agenda" "doh" "celeb" "all" "aback" "arab" "alb" "aga" "elba" "docs" "abate"
 "cinema")
