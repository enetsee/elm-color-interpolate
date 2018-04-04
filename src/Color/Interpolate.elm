module Color.Interpolate exposing (rgb, rgbBasis, rgbBasisClosed, hsl, hslLong)

{-|
@docs rgb, rgbBasis, rgbBasisClosed
@docs hsl, hslLong
-}

import Color exposing (Color, toHsl, toRgb)
import Array


-- RGB -------------------------------------------------------------------------


{-| -}
rgb :
    Maybe Float
    -> Color
    -> Color
    -> Float
    -> { red : Int, green : Int, blue : Int, alpha : Float }
rgb maybeGamma start stop =
    let
        color =
            Maybe.map gamma maybeGamma
                |> Maybe.withDefault noGamma

        a2 =
            toRgb start

        b2 =
            toRgb stop

        alphaComponent =
            color a2.alpha b2.alpha

        component f =
            round << color (toFloat <| f a2) (toFloat <| f b2)
    in
        \t ->
            { red = component .red t
            , green = component .green t
            , blue = component .blue t
            , alpha = alphaComponent t
            }


{-| -}
rgbBasis :
    List Color
    -> Float
    -> { alpha : Float, blue : Int, green : Int, red : Int }
rgbBasis =
    rgbSpline basis


{-| -}
rgbBasisClosed :
    List Color
    -> Float
    -> { alpha : Float, blue : Int, green : Int, red : Int }
rgbBasisClosed =
    rgbSpline basisClosed



-- HSL -------------------------------------------------------------------------


{-| -}
hsl :
    Color
    -> Color
    -> Float
    -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
hsl =
    hslHelper hue


{-| -}
hslLong :
    Color
    -> Color
    -> Float
    -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
hslLong =
    hslHelper noGamma



-- INTERNAL --------------------------------------------------------------------


rgbSpline :
    (List Float -> a -> Float)
    -> List Color
    -> a
    -> { alpha : Float, blue : Int, green : Int, red : Int }
rgbSpline spline colours =
    let
        ( rs, gs, bs, alphs ) =
            colours
                |> List.map
                    (\c ->
                        let
                            rgb =
                                toRgb c
                        in
                            ( toFloat rgb.red
                            , toFloat rgb.green
                            , toFloat rgb.blue
                            , rgb.alpha
                            )
                    )
                |> unzip4

        r =
            round << spline rs

        g =
            round << spline gs

        b =
            round << spline bs

        a =
            spline alphs
    in
        \t ->
            { red = r t
            , green = g t
            , blue = b t
            , alpha = a t
            }


hslHelper :
    (Float -> Float -> Float -> Float)
    -> Color
    -> Color
    -> Float
    -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
hslHelper hue a b =
    let
        a2 =
            toHsl a

        b2 =
            toHsl b

        hueComponent =
            hue a2.hue b2.hue

        saturationComponent =
            noGamma a2.saturation b2.saturation

        lightnessComponent =
            noGamma a2.lightness b2.lightness

        alphaComponent =
            noGamma a2.alpha b2.alpha
    in
        \t ->
            { hue = hueComponent t
            , saturation = saturationComponent t
            , lightness = lightnessComponent t
            , alpha = alphaComponent t
            }


unzip4 : List ( a, b, c, d ) -> ( List a, List b, List c, List d )
unzip4 quads =
    let
        step ( x, y, z, u ) ( xs, ys, zs, us ) =
            ( x :: xs, y :: ys, z :: zs, u :: us )
    in
        List.foldr step ( [], [], [], [] ) quads


hue : Float -> Float -> (Float -> Float)
hue h1 h2 =
    let
        h =
            h2 - h1 |> norm
    in
        if h == 0 then
            if isNaN h1 then
                always h2
            else
                always h1
        else
            linear h1 h


gamma : Float -> number -> number -> Float -> Float
gamma y =
    if y == 1.0 then
        noGamma
    else
        \a b ->
            if b - a /= 0 then
                exponential a b y
            else
                always <|
                    if isNaN a then
                        b
                    else
                        a


noGamma : number -> number -> number2 -> number2
noGamma a b =
    let
        d =
            b - a
    in
        if d /= 0 then
            linear a d
        else if isNaN a then
            always b
        else
            always a


exponential : Float -> Float -> Float -> Float -> Float
exponential a b y =
    let
        a2 =
            a ^ y

        b2 =
            (b ^ y) - a2

        y2 =
            1.0 / y
    in
        \t -> (a2 + t * b2) ^ y2


norm : Float -> Float
norm hue =
    hue - turns (toFloat (floor (hue / (2 * pi))))


linear : number -> number -> number -> number
linear a d t =
    a + t * d


{-| -}
basis : List Float -> Float -> Float
basis ls =
    let
        values =
            Array.fromList ls

        n =
            Array.length values - 1

        n2 =
            toFloat n
    in
        \tIn ->
            let
                ( i, t ) =
                    if tIn <= 0.0 then
                        ( 0, 0.0 )
                    else if tIn >= 1.0 then
                        ( 1, toFloat (n - 1) )
                    else
                        ( floor (tIn * n2), tIn )

                ms =
                    case ( Array.get i values, Array.get (i + 1) values ) of
                        ( Just v1, Just v2 ) ->
                            Maybe.map2
                                (\v0 v3 ->
                                    let
                                        m =
                                            (t - toFloat i / n2) * n2
                                    in
                                        basisHelper m v0 v1 v2 v3
                                )
                                (if i > 0 then
                                    Array.get (i - 1) values
                                 else
                                    Just (2.0 * v1 - v2)
                                )
                                (if i < n - 1 then
                                    Array.get (i + 2) values
                                 else
                                    Just (2.0 * v2 - v1)
                                )

                        _ ->
                            Nothing
            in
                case ms of
                    Just x ->
                        x

                    _ ->
                        Debug.crash "bang"


{-| -}
basisClosed : List Float -> Float -> Float
basisClosed ls =
    let
        values =
            Array.fromList ls

        n =
            Array.length values

        n2 =
            toFloat n
    in
        \tIn ->
            let
                temp =
                    fmod tIn 1

                ( t, i ) =
                    if temp < 0.0 then
                        ( temp + 1.0, floor (n2 * (temp + 1.0)) )
                    else
                        ( temp, floor (n2 * temp) )

                ms =
                    Maybe.map4
                        (\v0 v1 v2 v3 ->
                            basisHelper ((t - toFloat i / n2) * n2) v0 v1 v2 v3
                        )
                        (Array.get ((i + n - 1) % n) values)
                        (Array.get (i % n) values)
                        (Array.get ((i + 1) % n) values)
                        (Array.get ((i + 2) % n) values)
            in
                case ms of
                    Just v ->
                        v

                    _ ->
                        Debug.crash "bang"


basisHelper : Float -> Float -> Float -> Float -> Float -> Float
basisHelper t1 v0 v1 v2 v3 =
    let
        t2 =
            t1 * t1

        t3 =
            t2 * t1
    in
        ((1 - 3 * t1 + 3 * t2 - t3)
            * v0
            + (4 - 6 * t2 + 3 * t3)
            * v1
            + (1 + 3 * t1 + 3 * t2 - 3 * t3)
            * v2
            + t3
            * v3
        )
            / 6


fmod : Float -> Int -> Float
fmod f n =
    let
        integer =
            floor f
    in
        toFloat (integer % n) + f - toFloat integer


radToDeg : Float -> Float
radToDeg rads =
    rads * 180.0 / pi
