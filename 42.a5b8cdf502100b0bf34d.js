(window.webpackJsonp=window.webpackJsonp||[]).push([[42],{"UJR+":function(e,t,a){"use strict";var l,n=a("H2/w"),d=a("+rx9"),s=a("6g2T"),o=a("5L+5"),u=a("K7te"),r=a("7HZk"),i=a("avNA"),c=a("z85V"),p=a("O43b"),f=r.createComponent("SelectExample"),b=(l=[{label:"Select your car...",value:""},{label:"Volvo",value:"volvo"},{label:"Saab",value:"saab"},{label:"Mercedes",value:"mercedes"},{label:"Audi",value:"audi"}],r.make()(f)({initialState:{selectedOption:""},render:function(e){return s.column_([o.example(u.nativeSelect({disabled:u.defaults.disabled,name:u.defaults.name,onChange:p.handler(i.targetValue)((function(t){return e.setState((function(e){return{selectedOption:n.fromMaybe("")(t)}}))})),options:l,optionStyle:u.defaults.optionStyle,placeholder:u.defaults.placeholder,required:u.defaults.required,style:u.defaults.style,testId:u.defaults.testId,value:e.state.selectedOption})),o.example(u.nativeSelect({disabled:u.defaults.disabled,name:u.defaults.name,onChange:p.handler(i.targetValue)((function(t){return e.setState((function(e){return{selectedOption:n.fromMaybe("")(t)}}))})),options:l,optionStyle:u.defaults.optionStyle,placeholder:u.defaults.placeholder,required:u.defaults.required,style:c.css({textAlignLast:"right"}),testId:u.defaults.testId,value:e.state.selectedOption}))])}})(d.unit));e.exports={component:f,docs:b}}}]);