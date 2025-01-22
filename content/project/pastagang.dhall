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
        , title
        , artist = "pastagang"
        , tags
        , date
        }

let medias =
      [ mk
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
          [ "slow", "elevator" ]
      , mk
          "morning-coffee"
          2024-12-27
          "Morning Cofee"
          (None Text)
          [ "elevator" ]
      , mk "haunted-jam" 2024-12-28 "Haunted Jam" (None Text) [ "impro" ]
      , mk
          "polynudel"
          2025-01-18
          "polynudel"
          (Some "/audio/pgPolynudel")
          [ "club", "beat" ]
      , mk "sand-in" 2025-01-22 "Sand In" (Some "/audio/pgSandIn") [ "beat" ]
      ]

in  { name = "Pastagang", medias }
