(window.webpackJsonp=window.webpackJsonp||[]).push([[52],{"tB/v":function(e,t,a){"use strict";var s,n=a("sAO9"),r=a("rkps"),i=a("UhtL"),o=a("AKjc"),p=a("jcJx"),l=a("Tkt6"),d=a("+xh4"),c=a("QAgu"),m=a("Woux"),u=a("mjQF"),y=a("mLKm"),g=a("Fwyz"),x=a("PCwQ"),_=a("qajM"),h=a("FFyt"),b=a("HB2f"),k=a("coXC"),f=_.createComponent("ToastExample"),w=(s={message:r.Nothing.value,pendingMessage:"This is an example message",key:""},_.make()(f)({initialState:s,render:function(e){return m.column_([u.example(x.toastBubble({message:e.state.pendingMessage})),g.text({children:[h.text("Click here to trigger a toast notification: "),c.buttonGroup({children:[y.input({type:y.text_.type,autoComplete:y.text_.autoComplete,checked:y.text_.checked,disabled:y.text_.disabled,max:y.text_.max,min:y.text_.min,step:y.text_.step,name:y.text_.name,onBlur:y.text_.onBlur,onChange:k.handler(b.targetValue)(function(t){return e.setState(function(e){return{pendingMessage:r.fromMaybe("")(t),key:e.key,message:e.message}})}),onFocus:y.text_.onFocus,onKeyUp:y.text_.onKeyUp,pattern:y.text_.pattern,placeholder:y.text_.placeholder,readOnly:y.text_.readOnly,required:y.text_.required,size:y.text_.size,style:y.text_.style,testId:y.text_.testId,value:e.state.pendingMessage,variant:y.text_.variant}),d.button({accessibilityLabel:d.primary.accessibilityLabel,color:d.primary.color,disabled:d.primary.disabled,onPress:k.handler_(function(){var t=n.map(p.functorEffect)(i.show(i.showNumber))(l.random)();return e.setState(function(a){return{pendingMessage:a.pendingMessage,key:t,message:new r.Just(e.state.pendingMessage)}})()}),size:d.primary.size,style:d.primary.style,testId:d.primary.testId,title:"Notify",type:d.primary.type,loading:d.primary.loading})],joined:!0,style:h.css({})}),x.toastManager,_.keyed(e.state.key)(x.toast({message:e.state.message}))],className:g.p.className,color:g.p.color,style:g.p.style,tag:g.p.tag,testId:g.p.testId})])}})(o.unit));e.exports={component:f,docs:w};a("sygH")}}]);