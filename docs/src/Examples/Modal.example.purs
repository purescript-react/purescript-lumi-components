module Lumi.Components.Examples.Modal where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Nullable (notNull, null)
import Lumi.Components.Button as Button
import Lumi.Components.Button (button, secondary)
import Lumi.Components.Column (column_)
import Lumi.Components.Modal (dialog, errorModal, modal, modalTitle)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Text (body_)
import Lumi.Components.Example (example)
import React.Basic.Classic (Component, JSX, createComponent, fragment, make)
import React.Basic.DOM.Events (capture_)

data ModalTestId
  = SmallModal
  | MediumModal
  | LargeModal
  | ExtraLargeModal
  | ExtraExtraLargeModal
  | DialogModal
  | NoCloseButtonModal
  | LongModal
  | ErrorModal
  | ErrorModalWithAction
  | TopAlignedModal

derive instance eqModalId :: Eq ModalTestId

component :: Component Unit
component = createComponent "ModalExample"

docs :: JSX
docs = unit # make component
  { initialState:
      { modalId: Nothing
      , clicks: 0
      }

  , render: \self ->
      column_
        [ body_ $ "Number of clicks: " <> show self.state.clicks

        , example $
            button secondary
              { onPress = capture_ $ self.setState _ { modalId = Just SmallModal }
              , title = "Open modal, small"
              }

        , guard (self.state.modalId == Just SmallModal) $
            modal
              { modalOpen: true
              , closeButton: true
              , onRequestClose: self.setState _ { modalId = Nothing }
              , onActionButtonClick: notNull $ self.setState \state -> state { clicks = state.clicks + 1 }
              , actionButtonTitle: "Add clicks"
              , actionButtonState: Button.Enabled
              , size: Small
              , title: modalTitle "Modal title -- Small"
              , variant: ""
              , internalBorders: false
              , children:
                  fragment
                    [ body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Demo of event bubbling through Portals. Want to add more clicks?"
                    ]
              }

        , example $
            button secondary
              { onPress = capture_ $ self.setState _ { modalId = Just MediumModal }
              , title = "Open modal, medium, action button disabled"
              }

        , guard (self.state.modalId == Just MediumModal) $
            modal
              { modalOpen: true
              , closeButton: true
              , onRequestClose: self.setState _ { modalId = Nothing }
              , onActionButtonClick: notNull $ self.setState \state -> state { clicks = state.clicks + 1 }
              , actionButtonTitle: "Add clicks"
              , actionButtonState: Button.Disabled
              , size: Medium
              , title: modalTitle "Modal title -- Medium"
              , variant: ""
              , internalBorders: false
              , children:
                  body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
              }

        , example $
            button secondary
              { onPress = capture_ $ self.setState _ { modalId = Just MediumModal }
              , title = "Open modal, medium, action button loading"
              }

        , guard (self.state.modalId == Just MediumModal) $
            modal
              { modalOpen: true
              , closeButton: true
              , onRequestClose: self.setState _ { modalId = Nothing }
              , onActionButtonClick: notNull $ self.setState \state -> state { clicks = state.clicks + 1 }
              , actionButtonTitle: ""
              , actionButtonState: Button.Loading
              , size: Medium
              , title: modalTitle "Modal title -- Medium"
              , variant: ""
              , internalBorders: false
              , children:
                  body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
              }

        , example $
            button secondary
              { onPress = capture_ $ self.setState _ { modalId = Just LargeModal }
              , title = "Open modal, large"
              }

        , guard (self.state.modalId == Just LargeModal) $
            modal
              { modalOpen: true
              , closeButton: true
              , onRequestClose: self.setState _ { modalId = Nothing }
              , onActionButtonClick: notNull $ self.setState \state -> state { clicks = state.clicks + 1 }
              , actionButtonTitle: "Add clicks"
              , actionButtonState: Button.Enabled
              , size: Large
              , title: modalTitle "Modal title -- Large"
              , variant: ""
              , internalBorders: false
              , children:
                  fragment
                    [ body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    ]
              }

        , example $
            button secondary
              { onPress = capture_ $ self.setState _ { modalId = Just ExtraLargeModal }
              , title = "Open modal, extra large"
              }

        , guard (self.state.modalId == Just ExtraLargeModal) $
            modal
              { modalOpen: true
              , closeButton: true
              , onRequestClose: self.setState _ { modalId = Nothing }
              , onActionButtonClick: notNull $ self.setState \state -> state { clicks = state.clicks + 1 }
              , actionButtonTitle: "Add clicks"
              , actionButtonState: Button.Enabled
              , size: ExtraLarge
              , title: modalTitle "Modal title -- Extra Large"
              , variant: ""
              , internalBorders: false
              , children:
                  fragment
                    [ body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    ]
              }

        , example $
            button secondary
              { onPress = capture_ $ self.setState _ { modalId = Just ExtraExtraLargeModal }
              , title = "Open modal, extra extra large"
              }

        , guard (self.state.modalId == Just ExtraExtraLargeModal) $
            modal
              { modalOpen: true
              , closeButton: true
              , onRequestClose: self.setState _ { modalId = Nothing }
              , onActionButtonClick: notNull $ self.setState \state -> state { clicks = state.clicks + 1 }
              , actionButtonTitle: "Add clicks"
              , actionButtonState: Button.Enabled
              , size: ExtraExtraLarge
              , title: modalTitle "Modal title -- Extra Extra Large"
              , variant: ""
              , internalBorders: false
              , children:
                  fragment
                    [ body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    ]
              }

        , example $
            button secondary
              { onPress = capture_ $ self.setState _ { modalId = Just DialogModal }
              , title = "Open modal, dialog"
              }

        , guard (self.state.modalId == Just DialogModal) $
            dialog
              { modalOpen: true
              , onRequestClose: self.setState _ { modalId = Nothing }
              , onActionButtonClick: notNull $ self.setState \state -> state { clicks = state.clicks + 1 }
              , actionButtonTitle: "Add clicks"
              , size: Small
              , children:
                  body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
              }

        , example $
            button secondary
              { onPress = capture_ $ self.setState _ { modalId = Just NoCloseButtonModal }
              , title = "Open modal, no close button"
              }

        , guard (self.state.modalId == Just NoCloseButtonModal) $
            modal
              { modalOpen: true
              , closeButton: false
              , onRequestClose: self.setState _ { modalId = Nothing }
              , onActionButtonClick: notNull $ self.setState \state -> state { clicks = state.clicks + 1 }
              , actionButtonTitle: "Add clicks"
              , actionButtonState: Button.Enabled
              , size: Small
              , title: modalTitle "Modal title -- Small"
              , variant: ""
              , internalBorders: false
              , children:
                  fragment
                    [ body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Demo of event bubbling through Portals. Want to add more clicks?"
                    ]
              }

        , example $
            button secondary
              { onPress = capture_ $ self.setState _ { modalId = Just LongModal }
              , title = "Open modal, internal scrolling"
              }

        , guard (self.state.modalId == Just LongModal) $
            modal
              { modalOpen: true
              , closeButton: true
              , onRequestClose: self.setState _ { modalId = Nothing }
              , onActionButtonClick: notNull $ self.setState \state -> state { clicks = state.clicks + 1 }
              , actionButtonTitle: "Add clicks"
              , actionButtonState: Button.Enabled
              , size: Large
              , title: modalTitle "Modal title -- Internall scrolling content"
              , variant: "internal-scrolling"
              , internalBorders: true
              , children:
                  fragment
                    [ body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    , body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    ]
              }

        , example $
            button secondary
              { onPress = capture_ $ self.setState _ { modalId = Just ErrorModal }
              , title = "Open error modal, no action button"
              }

        , guard (self.state.modalId == Just ErrorModal) $
            errorModal
              { modalOpen: true
              , onRequestClose: self.setState _ { modalId = Nothing }
              , onActionButtonClick: null
              , actionButtonTitle: ""
              , children: body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
              }

        , example $
            button secondary
              { onPress = capture_ $ self.setState _ { modalId = Just ErrorModalWithAction }
              , title = "Open error modal, with action button"
              }

        , guard (self.state.modalId == Just ErrorModalWithAction) $
            errorModal
              { modalOpen: true
              , onRequestClose: self.setState _ { modalId = Nothing }
              , onActionButtonClick: notNull $ self.setState \state -> state { clicks = state.clicks + 1 }
              , actionButtonTitle: "Send"
              , children: body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
              }

        , example $
            button secondary
              { onPress = capture_ $ self.setState _ { modalId = Just TopAlignedModal }
              , title = "Open top-aligned modal"
              }

        , guard (self.state.modalId == Just TopAlignedModal) $
            modal
              { modalOpen: true
              , closeButton: true
              , onRequestClose: self.setState _ { modalId = Nothing }
              , onActionButtonClick: notNull $ self.setState \state -> state { clicks = state.clicks + 1 }
              , actionButtonTitle: "Add clicks"
              , actionButtonState: Button.Enabled
              , size: Large
              , title: modalTitle "Modal title -- Top-aligned content"
              , variant: "top-aligned"
              , internalBorders: true
              , children:
                  fragment
                    [ body_ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."
                    ]
              }
      ]
  }
