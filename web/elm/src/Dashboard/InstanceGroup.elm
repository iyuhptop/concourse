module Dashboard.InstanceGroup exposing (cardView)

import Application.Models exposing (Session)
import Assets
import Concourse
import Concourse.BuildStatus exposing (BuildStatus(..))
import Concourse.PipelineStatus as PipelineStatus
import Dashboard.Group.Models exposing (Pipeline)
import Dashboard.Pipeline exposing (pipelineStatus)
import Dashboard.Styles as Styles
import Dict exposing (Dict)
import Duration
import HoverState
import Html exposing (Html)
import Html.Attributes
    exposing
        ( attribute
        , class
        , classList
        , draggable
        , href
        , id
        , style
        )
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Message.Effects as Effects
import Message.Message exposing (DomID(..), Message(..), PipelinesSection(..))
import Routes
import Set
import SideBar.SideBar as SideBar
import Time
import UserState
import Views.FavoritedIcon
import Views.Icon as Icon
import Views.PauseToggle as PauseToggle
import Views.Spinner as Spinner
import Views.Styles



--hdPipelineView :
--    { pipeline : Pipeline
--    , pipelineRunningKeyframes : String
--    , resourceError : Bool
--    , existingJobs : List Concourse.Job
--    }
--    -> Html Message
--hdPipelineView { pipeline, pipelineRunningKeyframes, resourceError, existingJobs } =
--    Html.a
--        ([ class "card"
--         , attribute "data-pipeline-name" pipeline.name
--         , attribute "data-team-name" pipeline.teamName
--         , onMouseEnter <| TooltipHd pipeline.name pipeline.teamName
--         , href <| Routes.toString <| Routes.pipelineRoute pipeline
--         ]
--            ++ Styles.pipelineCardHd (pipelineStatus existingJobs pipeline)
--        )
--    <|
--        [ Html.div
--            (if pipeline.stale then
--                Styles.pipelineCardBannerStaleHd
--
--             else if pipeline.archived then
--                Styles.pipelineCardBannerArchivedHd
--
--             else
--                Styles.pipelineCardBannerHd
--                    { status = pipelineStatus existingJobs pipeline
--                    , pipelineRunningKeyframes = pipelineRunningKeyframes
--                    }
--            )
--            []
--        , Html.div
--            (class "dashboardhd-pipeline-name" :: Styles.pipelineCardBodyHd)
--            [ Html.text pipeline.name ]
--        ]
--            ++ (if resourceError then
--                    [ Html.div Styles.resourceErrorTriangle [] ]
--
--                else
--                    []
--               )


cardView :
    Session
    ->
        { pipeline : Pipeline
        , pipelines : List Pipeline
        , hovered : HoverState.HoverState
        , pipelineRunningKeyframes : String
        , resourceError : Bool
        , pipelineJobs : Dict Concourse.DatabaseID (List Concourse.JobIdentifier)
        , jobs : Dict ( Concourse.DatabaseID, String ) Concourse.Job
        , section : PipelinesSection
        }
    -> Html Message
cardView session { pipeline, pipelines, hovered, pipelineRunningKeyframes, resourceError, pipelineJobs, jobs, section } =
    --let
    --    bannerStyle =
    --        if pipeline.stale then
    --            Styles.pipelineCardBannerStale
    --        else if pipeline.archived then
    --            Styles.pipelineCardBannerArchived
    --        else
    --            Styles.pipelineCardBanner
    --                { status = pipelineStatus existingJobs pipeline
    --                , pipelineRunningKeyframes = pipelineRunningKeyframes
    --                }
    --in
    Html.div
        (Styles.instanceGroupCard
            -- TODO
            ++ (if section == AllPipelinesSection && not pipeline.stale then
                    [ style "cursor" "move" ]

                else
                    []
               )
            ++ (if pipeline.stale then
                    [ style "opacity" "0.45" ]

                else
                    []
               )
        )
        [ Html.div (class "banner" :: Styles.instanceGroupCardBanner) []
        , headerView pipeline pipelines resourceError
        , bodyView pipelineRunningKeyframes section hovered (pipeline :: pipelines) pipelineJobs jobs
        , footerView
        ]


headerView : Pipeline -> List Pipeline -> Bool -> Html Message
headerView pipeline pipelines resourceError =
    Html.a
        -- TODO
        [ href <| Routes.toString <| Routes.pipelineRoute pipeline, draggable "false" ]
        [ Html.div
            ([ class "card-header"
             , onMouseEnter <| Tooltip pipeline.name pipeline.teamName
             ]
                ++ Styles.instanceGroupCardHeader
            )
            [ badgeView (pipeline :: pipelines)
            , Html.div
                (class "dashboard-group-name" :: Styles.instanceGroupName)
                [ Html.text pipeline.name
                ]
            , Html.div
                [ classList [ ( "dashboard-resource-error", resourceError ) ] ]
                []
            ]
        ]


badgeView : List Pipeline -> Html Message
badgeView pipelines =
    let
        numPipelines =
            List.length pipelines

        ( text, fontSize ) =
            if numPipelines <= 99 then
                ( String.fromInt numPipelines, "14px" )

            else
                ( "99+", "12px" )
    in
    Html.div
        (Styles.instanceGroupCardHeaderBadge
            ++ [ style "font-size" fontSize ]
        )
        [ Html.text text ]


bodyView :
    String
    -> PipelinesSection
    -> HoverState.HoverState
    -> List Pipeline
    -> Dict Concourse.DatabaseID (List Concourse.JobIdentifier)
    -> Dict ( Concourse.DatabaseID, String ) Concourse.Job
    -> Html Message
bodyView pipelineRunningKeyframes section hovered pipelines pipelineJobs jobs =
    Html.div
        (class "card-body" :: Styles.instanceGroupCardBody)
        (pipelines
            |> List.map
                (\p ->
                    let
                        pipelinePage =
                            Routes.toString <|
                                Routes.pipelineRoute p

                        curPipelineJobs =
                            Dict.get p.id pipelineJobs
                                |> Maybe.withDefault []
                                |> List.filterMap
                                    (\{ pipelineId, jobName } ->
                                        Dict.get
                                            ( pipelineId, jobName )
                                            jobs
                                    )
                    in
                    Html.div
                        (Styles.instanceGroupCardPipelineBox
                            pipelineRunningKeyframes
                            (HoverState.isHovered
                                (PipelinePreview section p.id)
                                hovered
                            )
                            (pipelineStatus curPipelineJobs p)
                            ++ [ onMouseEnter <| Hover <| Just <| PipelinePreview section p.id
                               , onMouseLeave <| Hover Nothing
                               ]
                        )
                        [ Html.a
                            [ href pipelinePage
                            , style "flex-grow" "1"
                            ]
                            []
                        ]
                )
        )


footerView : Html Message
footerView =
    Html.div Styles.cardFooter []


circleSize : Float -> Float -> Int -> Float
circleSize width height n =
    let
        optimalRowsAndCols r c =
            if r * c >= n then
                ( r, c )

            else if height / (toFloat r + 1) >= width / (toFloat c + 1) then
                optimalRowsAndCols (r + 1) c

            else
                optimalRowsAndCols r (c + 1)

        ( rows, cols ) =
            optimalRowsAndCols 1 1
    in
    -- TODO: floor it?
    min (height / toFloat rows) (width / toFloat cols)
