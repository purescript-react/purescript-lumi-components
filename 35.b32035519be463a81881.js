(window.webpackJsonp=window.webpackJsonp||[]).push([[35],{aeAz:function(e,i,t){"use strict";var a=t("zo+L"),s=t("rkps"),l=t("T4xb"),n=t("PA/0"),u=t("UhtL"),o=t("AKjc"),c=t("+xh4"),r=t("Woux"),d=t("mjQF"),m=t("N8yC"),p=t("pkhx"),f=t("Fwyz"),b=t("qajM"),y=t("HB2f"),g=function(){function e(){}return e.value=new e,e}(),I=function(){function e(){}return e.value=new e,e}(),v=function(){function e(){}return e.value=new e,e}(),q=function(){function e(){}return e.value=new e,e}(),P=function(){function e(){}return e.value=new e,e}(),k=function(){function e(){}return e.value=new e,e}(),h=function(){function e(){}return e.value=new e,e}(),L=new a.Eq(function(e){return function(i){return e instanceof g&&i instanceof g||(e instanceof I&&i instanceof I||(e instanceof v&&i instanceof v||(e instanceof q&&i instanceof q||(e instanceof P&&i instanceof P||(e instanceof k&&i instanceof k||e instanceof h&&i instanceof h)))))}}),S=b.createComponent("ModalExample"),M=b.make()(S)({initialState:{modalId:s.Nothing.value,clicks:0},render:function(e){return r.column_([f.body_("Number of clicks: "+u.show(u.showInt)(e.state.clicks)),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,disabled:c.secondary.disabled,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(g.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, small",type:c.secondary.type,loading:c.secondary.loading})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(L))(e.state.modalId)(new s.Just(g.value)))(m.modal({modalOpen:!0,closeButton:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Add clicks",actionButtonDisabled:!1,size:p.Small.value,title:"Modal title -- Small",variant:"",internalBorders:!1,children:b.fragment([f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Demo of event bubbling through Portals. Want to add more clicks?")])})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,disabled:c.secondary.disabled,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(I.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, medium, action button disabled",type:c.secondary.type,loading:c.secondary.loading})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(L))(e.state.modalId)(new s.Just(I.value)))(m.modal({modalOpen:!0,closeButton:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Add clicks",actionButtonDisabled:!0,size:p.Medium.value,title:"Modal title -- Medium",variant:"",internalBorders:!1,children:f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante.")})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,disabled:c.secondary.disabled,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(v.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, large",type:c.secondary.type,loading:c.secondary.loading})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(L))(e.state.modalId)(new s.Just(v.value)))(m.modal({modalOpen:!0,closeButton:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Add clicks",actionButtonDisabled:!1,size:p.Large.value,title:"Modal title -- Large",variant:"",internalBorders:!1,children:b.fragment([f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante.")])})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,disabled:c.secondary.disabled,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(q.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, dialog",type:c.secondary.type,loading:c.secondary.loading})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(L))(e.state.modalId)(new s.Just(q.value)))(m.dialog({modalOpen:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Add clicks",size:p.Small.value,children:f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit.")})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,disabled:c.secondary.disabled,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(q.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, no close button",type:c.secondary.type,loading:c.secondary.loading})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(L))(e.state.modalId)(new s.Just(q.value)))(m.modal({modalOpen:!0,closeButton:!1,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Add clicks",actionButtonDisabled:!1,size:p.Small.value,title:"Modal title -- Small",variant:"",internalBorders:!1,children:b.fragment([f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Demo of event bubbling through Portals. Want to add more clicks?")])})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,disabled:c.secondary.disabled,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(P.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, internal scrolling",type:c.secondary.type,loading:c.secondary.loading})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(L))(e.state.modalId)(new s.Just(P.value)))(m.modal({modalOpen:!0,closeButton:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Add clicks",actionButtonDisabled:!1,size:p.Large.value,title:"Modal title -- Internall scrolling content",variant:"internal-scrolling",internalBorders:!0,children:b.fragment([f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante.")])})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,disabled:c.secondary.disabled,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(k.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open error modal, no action button",type:c.secondary.type,loading:c.secondary.loading})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(L))(e.state.modalId)(new s.Just(k.value)))(m.errorModal({modalOpen:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.null,actionButtonTitle:"",children:f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit.")})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,disabled:c.secondary.disabled,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(h.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open error modal, with action button",type:c.secondary.type,loading:c.secondary.loading})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(L))(e.state.modalId)(new s.Just(h.value)))(m.errorModal({modalOpen:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Send",children:f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit.")}))])}})(o.unit);e.exports={SmallModal:g,MediumModal:I,LargeModal:v,DialogModal:q,LongModal:P,ErrorModal:k,ErrorModalWithAction:h,component:S,docs:M,eqModalId:L};t("sygH")}}]);