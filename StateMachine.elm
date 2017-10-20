import Html exposing (text)

type Status = Draft | Published | Paused | Expired

updateStatus: Status -> Status -> Status
updateStatus oldStatus newStatus =
  let
    publish s = case s of
      Draft -> Published
      Paused -> Published
      _ -> s
    unpublish s = case s of
      Published -> Paused
      _ -> s
  in
  case newStatus of
    Published -> publish oldStatus
    Paused -> unpublish oldStatus
    _ -> oldStatus

main =
  text (toString (updateStatus Published Expired))
