(window.webpackJsonp=window.webpackJsonp||[]).push([[46],{Qy3S:function(e,t,r){"use strict";var n,a=r("jPEo"),s=r("XSw5"),o=r("gK7E"),l=r("H2/w"),i=r("U4xy"),u=r("ij/N"),c=r("RqP8"),p=r("+rx9"),m=r("SZBU"),d=r("6g2T"),g=r("5L+5"),y=r("ErWj"),f=r("+0be"),b=r("jEuo"),h=r("pYS8"),w=r("PsKH"),v=r("pMgY"),x=r("wSBi"),S=r("sfHK"),I=r("avNA"),L=r("z85V"),E=r("O43b"),C=function(){function e(e){this.value0=e}return e.create=function(t){return new e(t)},e}(),P=function(){function e(){}return e.value=new e,e}(),B=function(){function e(){}return e.value=new e,e}(),D=x.createComponent("ProgressExample"),F=(n=function(e){return function(t){if(t instanceof C)return e.setState((function(e){return{percent:t.value0}}));if(t instanceof P)return e.setState((function(e){return{percent:u.min(u.ordInt)(100)(e.percent+10|0)}}));if(t instanceof B)return e.setState((function(e){return{percent:0}}));throw new Error("Failed pattern match at Lumi.Components.Examples.Progress (line 84, column 17 - line 90, column 40): "+[t.constructor.name])}},x.make()(D)({initialState:{percent:20},render:function(e){return d.column_([d.column({style:L.css({maxWidth:"500px",padding:"20px 0"}),children:[f.labeledField({label:S.text("Adjust progress:"),value:h.row_([y.input({type:y.range.type,autoComplete:y.range.autoComplete,autoFocus:y.range.autoFocus,checked:y.range.checked,disabled:y.range.disabled,max:i.toNullable(new l.Just(100)),min:i.toNullable(new l.Just(0)),maxLength:y.range.maxLength,minLength:y.range.minLength,step:y.range.step,name:y.range.name,onBlur:y.range.onBlur,onChange:E.handler(I.targetValue)((t=n(e),r=l.fromMaybe(0),u=s.flip(a.bind(l.bindMaybe))(o.fromString),function(e){return t(C.create(r(u(e))))})),onFocus:y.range.onFocus,onKeyUp:y.range.onKeyUp,pattern:y.range.pattern,placeholder:y.range.placeholder,readOnly:y.range.readOnly,required:y.range.required,size:y.range.size,style:L.css({maxWidth:"200px"}),testId:y.range.testId,value:c.show(c.showInt)(e.state.percent),variant:y.range.variant}),m.button({accessibilityLabel:m.primary.accessibilityLabel,color:m.primary.color,onPress:E.handler_(n(e)(P.value)),size:w.Small.value,style:L.css({marginLeft:"16px"}),testId:m.primary.testId,title:"+10%",type:m.primary.type,buttonState:m.primary.buttonState}),m.button({accessibilityLabel:m.primary.accessibilityLabel,color:m.primary.color,onPress:E.handler_(n(e)(B.value)),size:w.Small.value,style:L.css({marginLeft:"8px"}),testId:m.primary.testId,title:"Reset",type:m.primary.type,buttonState:m.primary.buttonState})]),validationError:l.Nothing.value,required:f.Neither.value,forceTopLabel:!1,style:L.css({})})]}),v.h2_("Bar"),g.example(b.progressBar({total:b.progressDefaults.total,completed:e.state.percent,style:b.progressDefaults.style,testId:b.progressDefaults.testId})),v.h2_("Circle"),g.example(b.progressCircle({total:b.progressDefaults.total,completed:e.state.percent,style:b.progressDefaults.style,testId:b.progressDefaults.testId}))]);var t,r,u}})(p.unit));e.exports={component:D,Set:C,Increment:P,Reset:B,docs:F}}}]);