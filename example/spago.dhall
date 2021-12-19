let conf = ../spago.dhall

in      conf
    //  { name = conf.name ++ "-example"
        , dependencies =
              conf.dependencies
            # [ "halogen"
              , "halogen-hooks"
              , "halogen-storybook"
              , "foreign-object"
              , "web-html"
              ]
        , sources = conf.sources # [ "example/**/*.purs" ]
        }
