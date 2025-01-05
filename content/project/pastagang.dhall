let mk =
      \(name : Text) ->
      \(date : Date) ->
      \(title : Text) ->
      \(note : Optional Text) ->
      \(tags : List Text) ->
        { path = "/audio/2024-pastagang/${name}"
        , format = [ "flac" ]
        , cover = None Text
        , note
        , name
        , title
        , artist = "pastagang"
        , tags
        , date
        }

in  [ mk
        "farfalleFunk"
        2024-11-23
        "Farfalle Funk"
        (Some "/audio/pgFarfalleFunk")
        [ "deep" ]
    , mk
        "simple-but-hard"
        2024-11-25
        "Simple but Hard"
        (Some "/audio/pgSimpleButHard")
        [ "slow" ]
    , mk
        "madMontuno"
        2024-11-30
        "Mad Montuno"
        (Some "/audio/pgMadMontuno")
        [ "best" ]
    , mk
        "shaving-clouds"
        2024-12-08
        "Shaving Clouds"
        (Some "/audio/pgShavingClouds")
        [ "slow" ]
    , mk "haunted-jam" 2024-12-28 "Haunted Jam" (None Text) [ "impro" ]
    ]
