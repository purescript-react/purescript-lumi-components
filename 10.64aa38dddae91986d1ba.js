(window.webpackJsonp=window.webpackJsonp||[]).push([[10],{"2veT":function(e,t,n){"use strict";var r,o,i,u,a,c,s=n("NL7e"),l=n("jPEo"),m=n("rUU2"),d=n("qIyz"),p=n("tiZW"),b=n("CO8c"),y=n("6nUn"),f=n("5a5/"),w=n("ll6l"),q=n("IORp"),x=n("H2/w"),h=n("SXP8"),g=n("UMim"),N=n("FYgN"),v=n("d46P"),I=n("RqP8"),C=n("Qku0"),_=n("+rx9"),F=n("xQXF"),S=n("6g2T"),R=n("DabP"),E=n("5L+5"),k=n("ErWj"),A=n("pYS8"),B=n("pMgY"),U=n("7HZk"),M=n("sfHK"),O=n("avNA"),P=n("O43b"),T=U.createComponent("EditableTableExample"),z=(r=function(e){return function(t){return e.setState(function(n){return{rows:w.mapFlipped(d.functorNonEmptyArray)(e.state.rows)(function(e){return e.id===t.id?t:e})}})}},o=function(e){return"$"+N.toStringWith(N.fixed(2))(e)},i=function(e){return function(t){return e.setState(function(n){return{rows:function(){var n=m.fromArray(m.filter(q.not(q.heytingAlgebraFunction(q.heytingAlgebraFunction(q.heytingAlgebraBoolean)))(y.eq(y.eqRec()(y.eqRowCons(y.eqRowCons(y.eqRowCons(y.eqRowCons(y.eqRowNil)()(new C.IsSymbol(function(){return"quantity"}))(p.eqBigInt))()(new C.IsSymbol(function(){return"price"}))(y.eqNumber))()(new C.IsSymbol(function(){return"id"}))(y.eqInt))()(new C.IsSymbol(function(){return"description"}))(y.eqString))))(t))(e.state.rows));if(n instanceof x.Nothing)return e.state.rows;if(n instanceof x.Just)return n.value0;throw new Error("Failed pattern match at Lumi.Components.Examples.EditableTable (line 115, column 18 - line 117, column 24): "+[n.constructor.name])}()}})}},u={id:-1,description:"",quantity:p.fromInt(0),price:0},a=function(e){return p.toNumber(e.quantity)*e.price},c=function(e){return e.setState(function(t){return{rows:m.snoc(e.state.rows)({id:m.length(e.state.rows),description:u.description,price:u.price,quantity:u.quantity})}})},U.make()(T)({initialState:{rows:m.fromNonEmpty(new h.NonEmpty({id:0,description:"Boxes",quantity:p.fromInt(1e3),price:1.32},[{id:1,description:"Tape",quantity:p.fromInt(100),price:.23}]))},render:function(e){return S.column_([E.example(R.editableTable({addLabel:"Add another row",columns:[{label:"Description",renderCell:function(t){return k.input({type:k.text_.type,autoComplete:k.text_.autoComplete,autoFocus:k.text_.autoFocus,checked:k.text_.checked,disabled:k.text_.disabled,max:k.text_.max,min:k.text_.min,step:k.text_.step,name:k.text_.name,onBlur:k.text_.onBlur,onChange:P.handler(O.targetValue)(function(n){return r(e)({id:t.id,description:x.fromMaybe(t.description)(n),price:t.price,quantity:t.quantity})}),onFocus:k.text_.onFocus,onKeyUp:k.text_.onKeyUp,pattern:k.text_.pattern,placeholder:k.text_.placeholder,readOnly:k.text_.readOnly,required:k.text_.required,size:k.text_.size,style:M.css({width:"100%"}),testId:k.text_.testId,value:t.description,variant:k.text_.variant})}},{label:"Quantity",renderCell:function(t){return k.input({type:k.number.type,autoComplete:k.number.autoComplete,autoFocus:k.number.autoFocus,checked:k.number.checked,disabled:k.number.disabled,max:k.number.max,min:k.number.min,step:k.number.step,name:k.number.name,onBlur:k.number.onBlur,onChange:P.handler(O.targetValue)(function(n){return r(e)({id:t.id,description:t.description,price:t.price,quantity:x.fromMaybe(t.quantity)(l.bindFlipped(x.bindMaybe)(p.fromString)(n))})}),onFocus:k.number.onFocus,onKeyUp:k.number.onKeyUp,pattern:k.number.pattern,placeholder:k.number.placeholder,readOnly:k.number.readOnly,required:k.number.required,size:k.number.size,style:k.number.style,testId:k.number.testId,value:p.toString(t.quantity),variant:k.number.variant})}},{label:"Price",renderCell:function(t){return k.input({type:k.number.type,autoComplete:k.number.autoComplete,autoFocus:k.number.autoFocus,checked:k.number.checked,disabled:k.number.disabled,max:k.number.max,min:k.number.min,step:k.number.step,name:k.number.name,onBlur:k.number.onBlur,onChange:P.handler(O.targetValue)(function(n){return r(e)(x.fromMaybe(t)(l.bind(x.bindMaybe)(w.map(x.functorMaybe)(F.readInt(10))(n))(function(e){return s.pure(x.applicativeMaybe)(g.isNaN(e)?t:{price:e,description:t.description,id:t.id,quantity:t.quantity})})))}),onFocus:k.number.onFocus,onKeyUp:k.number.onKeyUp,pattern:k.number.pattern,placeholder:k.number.placeholder,readOnly:k.number.readOnly,required:k.number.required,size:k.number.size,style:k.number.style,testId:k.number.testId,value:I.show(I.showNumber)(t.price),variant:k.number.variant})}},{label:"Total",renderCell:function(e){return M.text(o(a(e)))}}],maxRows:5,onRowAdd:c(e),onRowRemove:i(e),readonly:!1,rowEq:y.eq(y.eqRec()(y.eqRowCons(y.eqRowCons(y.eqRowCons(y.eqRowCons(y.eqRowNil)()(new C.IsSymbol(function(){return"quantity"}))(p.eqBigInt))()(new C.IsSymbol(function(){return"price"}))(y.eqNumber))()(new C.IsSymbol(function(){return"id"}))(y.eqInt))()(new C.IsSymbol(function(){return"description"}))(y.eqString))),rows:new b.Right(e.state.rows),summary:A.row_([B.text({children:[M.text("Total:")],className:B.body.className,color:B.body.color,style:M.css({fontWeight:"bold"}),tag:B.body.tag,testId:B.body.testId}),B.body_(B.nbsp),B.body_(B.nbsp),B.body_(B.nbsp),B.body_(B.nbsp),B.body_(o(f.sum(d.foldableNonEmptyArray)(v.semiringNumber)(w.map(d.functorNonEmptyArray)(a)(e.state.rows))))])}))])}})(_.unit));e.exports={component:T,docs:z}},FYgN:function(e,t,n){"use strict";var r=n("wJdC"),o=n("ij/N"),i=function(){function e(e){this.value0=e}return e.create=function(t){return new e(t)},e}(),u=function(){function e(e){this.value0=e}return e.create=function(t){return new e(t)},e}(),a=function(){function e(e){this.value0=e}return e.create=function(t){return new e(t)},e}();e.exports={precision:function(e){return i.create(o.clamp(o.ordInt)(1)(21)(e))},fixed:function(e){return u.create(o.clamp(o.ordInt)(0)(20)(e))},exponential:function(e){return a.create(o.clamp(o.ordInt)(0)(20)(e))},toStringWith:function(e){if(e instanceof i)return r.toPrecisionNative(e.value0);if(e instanceof u)return r.toFixedNative(e.value0);if(e instanceof a)return r.toExponentialNative(e.value0);throw new Error("Failed pattern match at Data.Number.Format (line 59, column 1 - line 59, column 40): "+[e.constructor.name])},toString:r.toString}},wJdC:function(e,t){function n(e){return function(t){return function(n){return e.apply(n,[t])}}}t.toPrecisionNative=n(Number.prototype.toPrecision),t.toFixedNative=n(Number.prototype.toFixed),t.toExponentialNative=n(Number.prototype.toExponential),t.toString=function(e){return e.toString()}}}]);