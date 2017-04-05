# FbSR to FbOb translation
We are just going to encode objects on the fly as opposed to actually using classes. This is because the lifetime of the state or records is only the time of the expression itself.

1. toFbOb({label<sub>1</sub> = expression<sub>1</sub>; ... ; label<sub>n</sub> = expression<sub>n</sub>}) = <br/>
Object<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Inst<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;label<sub>1</sub> = toFbOb(expression<sub>1</sub>)<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;...<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;label<sub>n</sub> = toFbOb(expression<sub>n</sub>)<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Meth<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;getlabel<sub>1</sub> = ... (* getters for all the labels *) <br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;...<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;getlabel<sub>n</sub> = ...<br/>

2. toFbOb(e.label<sub>k</sub>) = toFbOb(e).getlabel<sub>k</sub><br/>
(* The "R" section of the FbSR to FbOb translator is done *)