(window.webpackJsonp=window.webpackJsonp||[]).push([[31],{t1Ft:function(e,t,i){"use strict";var a=i("6nUn"),s=i("H2/w"),l=i("W+DG"),n=i("U4xy"),u=i("RqP8"),o=i("+rx9"),c=i("SZBU"),r=i("6g2T"),d=i("5L+5"),m=i("fjFE"),p=i("PsKH"),f=i("pMgY"),b=i("7HZk"),y=i("avNA"),g=function(){function e(){}return e.value=new e,e}(),I=function(){function e(){}return e.value=new e,e}(),v=function(){function e(){}return e.value=new e,e}(),q=function(){function e(){}return e.value=new e,e}(),P=function(){function e(){}return e.value=new e,e}(),S=function(){function e(){}return e.value=new e,e}(),k=function(){function e(){}return e.value=new e,e}(),L=function(){function e(){}return e.value=new e,e}(),h=function(){function e(){}return e.value=new e,e}(),M=new a.Eq(function(e){return function(t){return e instanceof g&&t instanceof g||(e instanceof I&&t instanceof I||(e instanceof v&&t instanceof v||(e instanceof q&&t instanceof q||(e instanceof P&&t instanceof P||(e instanceof S&&t instanceof S||(e instanceof k&&t instanceof k||(e instanceof L&&t instanceof L||e instanceof h&&t instanceof h)))))))}}),B=b.createComponent("ModalExample"),w=b.make()(B)({initialState:{modalId:s.Nothing.value,clicks:0},render:function(e){return r.column_([f.body_("Number of clicks: "+u.show(u.showInt)(e.state.clicks)),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(g.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, small",type:c.secondary.type,buttonState:c.secondary.buttonState})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(M))(e.state.modalId)(new s.Just(g.value)))(m.modal({modalOpen:!0,closeButton:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Add clicks",actionButtonState:c.Enabled.value,size:p.Small.value,title:"Modal title -- Small",variant:"",internalBorders:!1,children:b.fragment([f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Demo of event bubbling through Portals. Want to add more clicks?")])})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(I.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, medium, action button disabled",type:c.secondary.type,buttonState:c.secondary.buttonState})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(M))(e.state.modalId)(new s.Just(I.value)))(m.modal({modalOpen:!0,closeButton:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Add clicks",actionButtonState:c.Disabled.value,size:p.Medium.value,title:"Modal title -- Medium",variant:"",internalBorders:!1,children:f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante.")})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(I.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, medium, action button loading",type:c.secondary.type,buttonState:c.secondary.buttonState})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(M))(e.state.modalId)(new s.Just(I.value)))(m.modal({modalOpen:!0,closeButton:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"",actionButtonState:c.Loading.value,size:p.Medium.value,title:"Modal title -- Medium",variant:"",internalBorders:!1,children:f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante.")})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(v.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, large",type:c.secondary.type,buttonState:c.secondary.buttonState})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(M))(e.state.modalId)(new s.Just(v.value)))(m.modal({modalOpen:!0,closeButton:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Add clicks",actionButtonState:c.Enabled.value,size:p.Large.value,title:"Modal title -- Large",variant:"",internalBorders:!1,children:b.fragment([f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante.")])})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(q.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, extra large",type:c.secondary.type,buttonState:c.secondary.buttonState})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(M))(e.state.modalId)(new s.Just(q.value)))(m.modal({modalOpen:!0,closeButton:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Add clicks",actionButtonState:c.Enabled.value,size:p.ExtraLarge.value,title:"Modal title -- Extra Large",variant:"",internalBorders:!1,children:b.fragment([f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante.")])})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(P.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, dialog",type:c.secondary.type,buttonState:c.secondary.buttonState})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(M))(e.state.modalId)(new s.Just(P.value)))(m.dialog({modalOpen:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Add clicks",size:p.Small.value,children:f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit.")})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(S.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, no close button",type:c.secondary.type,buttonState:c.secondary.buttonState})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(M))(e.state.modalId)(new s.Just(S.value)))(m.modal({modalOpen:!0,closeButton:!1,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Add clicks",actionButtonState:c.Enabled.value,size:p.Small.value,title:"Modal title -- Small",variant:"",internalBorders:!1,children:b.fragment([f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Demo of event bubbling through Portals. Want to add more clicks?")])})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(k.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open modal, internal scrolling",type:c.secondary.type,buttonState:c.secondary.buttonState})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(M))(e.state.modalId)(new s.Just(k.value)))(m.modal({modalOpen:!0,closeButton:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Add clicks",actionButtonState:c.Enabled.value,size:p.Large.value,title:"Modal title -- Internall scrolling content",variant:"internal-scrolling",internalBorders:!0,children:b.fragment([f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante."),f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam pretium nec tellus ornare tincidunt. Phasellus ultrices porta finibus. In id mollis diam. Praesent efficitur lectus quis odio convallis placerat. Suspendisse metus tortor, faucibus nec imperdiet quis, iaculis id risus. Pellentesque a auctor turpis, a lacinia nulla. Pellentesque malesuada suscipit ante, sed convallis est pharetra eu. In sed enim nec lacus dignissim malesuada. Lorem ipsum dolor sit amet, consectetur adipiscing elit. In metus arcu, efficitur et magna a, fermentum lacinia nulla. Mauris ligula erat, posuere sed diam a, sodales vestibulum ante.")])})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(L.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open error modal, no action button",type:c.secondary.type,buttonState:c.secondary.buttonState})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(M))(e.state.modalId)(new s.Just(L.value)))(m.errorModal({modalOpen:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.null,actionButtonTitle:"",children:f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit.")})),d.example(c.button({accessibilityLabel:c.secondary.accessibilityLabel,color:c.secondary.color,onPress:y.capture_(e.setState(function(e){return{modalId:new s.Just(h.value),clicks:e.clicks}})),size:c.secondary.size,style:c.secondary.style,testId:c.secondary.testId,title:"Open error modal, with action button",type:c.secondary.type,buttonState:c.secondary.buttonState})),l.guard(b.monoidJSX)(a.eq(s.eqMaybe(M))(e.state.modalId)(new s.Just(h.value)))(m.errorModal({modalOpen:!0,onRequestClose:e.setState(function(e){return{modalId:s.Nothing.value,clicks:e.clicks}}),onActionButtonClick:n.notNull(e.setState(function(e){return{modalId:e.modalId,clicks:e.clicks+1|0}})),actionButtonTitle:"Send",children:f.body_("Lorem ipsum dolor sit amet, consectetur adipiscing elit.")}))])}})(o.unit);e.exports={SmallModal:g,MediumModal:I,LargeModal:v,ExtraLargeModal:q,DialogModal:P,NoCloseButtonModal:S,LongModal:k,ErrorModal:L,ErrorModalWithAction:h,component:B,docs:w,eqModalId:M}}}]);