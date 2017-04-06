# FbSR to FbOb translation
We are just going to encode objects on the fly as opposed to actually using classes. This is because the lifetime of the state or records is only the time of the expression itself.<br/>
**Name:** Srinivas Suresh Kumar<br/>
**JHED:** ssures11<br/>
**Collaborators:** Rohit Ravoori, Alex Owen<br/>

**Note: There is nothing special about the getter and setter functions. They are absolutely the same ones from the textbook examples of FbOb. Therefore for brevity and neatness I am NOT redefining them here**

1. *toFbOb*({label<sub>1</sub> = expression<sub>1</sub>; ... ; label<sub>n</sub> = expression<sub>n</sub>}) = <br/>
**Object**<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Inst**<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;label<sub>1</sub> = *toFbOb*(expression<sub>1</sub>)<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;...<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;label<sub>n</sub> = *toFbOb*(expression<sub>n</sub>)<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Meth**<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;getlabel<sub>1</sub> = ... (* getters for all the labels *) <br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;...<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;getlabel<sub>n</sub> = ...<br/>

2. *toFbOb*(e.label<sub>k</sub>) = *toFbOb*(e).getlabel<sub>k</sub><br/>
(* The "R" section of the FbSR to FbOb translator is done *)
3. *toFbOb*(Ref e) =<br/>
**Object**<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Inst**<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Reference = *toFbOb*(e)<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**Meth**<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;getReference = ... (* Returns the reference*)<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;setReference = ... (* Assigns a value to reference*)

4. *toFbOb*(! e) = *toFbOb*(e).getReference
5. *toFbOb*(e<sub>1</sub> := e<sub>2</sub>) = *toFbOb*(e<sub>1</sub>).setReference(*toFbOb*(e<sub>2</sub>))
6. *All other rules for any expression are the same aka homomorphic*