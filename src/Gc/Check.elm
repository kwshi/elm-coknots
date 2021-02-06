module Gc.Check exposing (..)

import Dict
import Gc


type Err
    = CrossingVisits Int
    | InconsistentSigns (List Gc.Sign)
    | InconsistentOrders (List Gc.Order)


type alias Visit =
    { times : Int
    , signConsistent : Bool
    , orderConsistent : Bool
    , orders : List Gc.Order
    , signs : List Gc.Sign
    , sign : Gc.Sign
    , firstOrder : Gc.Order
    }


type alias Errs =
    Dict.Dict Int (List Err)


check : Gc.Gc -> Errs
check =
    List.foldl
        (\waypoint ( i, visits ) ->
            ( i + 1
            , Dict.update waypoint.label
                (Maybe.map
                    (\visit ->
                        { visit
                            | times = visit.times + 1
                            , signConsistent =
                                visit.signConsistent && waypoint.sign == visit.sign
                            , orderConsistent =
                                visit.orderConsistent && waypoint.order /= visit.firstOrder
                            , waypoints = waypoint :: visit.waypoints
                        }
                    )
                    >> Maybe.withDefault
                        { times = 1
                        , signConsistent = True
                        , orderConsistent = True
                        , waypoints = [ waypoint ]
                        , sign = waypoint.sign
                        , firstOrder = waypoint.order
                        }
                    >> Just
                )
                visits
            )
        )
        ( 0, Dict.empty )
        >> Tuple.second
        >> Dict.map
            (\_ visit ->
                [ if visit.times == 2 then
                    Nothing

                  else
                    Just <| CrossingVisits visit.times
                , if visit.signConsistent then
                    Nothing

                  else
                    Just <| InconsistentSigns <| List.map .sign visit.waypoints
                , if visit.orderConsistent then
                    Nothing

                  else
                    Just <| InconsistentOrders <| List.map .order visit.waypoints
                ]
                    |> List.filterMap identity
            )
