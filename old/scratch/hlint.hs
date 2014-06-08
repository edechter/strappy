<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
<meta http-equiv="Content-Type" content="application/xhtml+xml; charset=UTF-8" />
<title>HLint Report</title>
<script type='text/javascript'>

/* == Algorithm for show/unshow ==
   Each hint/file is given a number, hint# or file#
   When we say showOnly with a class name we add the rules to
   the css #content div {display:none}, #content div.className {display:block}
   When going back to showAll we remove these results
*/

// CSS MANIPULATION //

function deleteRules(n)
{
	var css = document.styleSheets[0];
	for (var i = 0; i < n; i++)
		css.deleteRule(css.cssRules.length-1);
}

function insertRule(s)
{
	var css = document.styleSheets[0];
	css.insertRule(s, css.cssRules.length);
}

// SHOW/HIDE LOGIC //

var last = "";

function show(id)
{
	if (id == last) return;
	if (id == "")
	{
		deleteRules(3);
		insertRule(".all {font-weight: bold;}");
	}
	else
	{
		if (last == "")
		{
			deleteRules(1);
			insertRule("#content div {display:none;}");
		}
		else
		{
			deleteRules(2);
		}
		insertRule("#content div." + id + " {display:block;}");
		insertRule("#" + id + "{font-weight:bold;}");
	}

	last = id;
}

</script>
<style type="text/css">
/* These rules are manipulated by the script.
   The commented form is how it looks with an id selected */

.all {font-weight: bold;}      /* #content div {display:none;} */
                               /* #content div.id {display:block;} */
                               /* #id {font-weight: bold;} */

</style>
<style type="text/css">
/* See http://www.webreference.com/programming/css_frames/ */
body {
	margin:0;
	border:0;
	padding:0;
	height:100%;
	max-height:100%;
	font-family: sans-serif;
	font-size:76%;
	overflow: hidden;
}

#leftbar {
	position:absolute;
	top:0px;
	left:0px;
	width: 215px;
	bottom: 0px;
	overflow:auto;
	background:rgb(202,223,255);
	margin: 10px;
	padding-top: 0;
	padding-left: 7px;
	padding-right: 7px;
	-moz-border-radius: 5px;
	-webkit-border-radius: 5px;

	display:none; /* Override if script present */
}

#content {
	position:absolute;
	top:0;
	bottom:0;
	right:0;
	overflow:auto;
	padding-bottom: 15px;
	padding-right: 7px;

	left:10px; /* Override if script present */
}

#leftbar ul {margin-top: 0px; padding-left: 15px;}
#leftbar p {margin-bottom: 0px;}
.note {color: gray; font-size: smaller;}

pre {
	font-family: "lucida console", monospace;
	padding-left: 15px;
	margin: 2px;
}

#content div {
	margin-bottom: 10px;
	margin-right: 10px;
	padding-top: 4px;
	border-top: 1px solid #ccc;
}

.script #content {left:250px;}
.script #leftbar {display: block;}

/* From HsColour */
.hs-keyglyph, .hs-layout {color: red;}
.hs-keyword {color: blue;}
.hs-comment, .hs-comment a {color: green;}
.hs-str, .hs-chr {color: teal;}
</style>
</head>
<body>

<script type='text/javascript'>
document.body.className = "script";
</script>

<div id="leftbar" valign="top" style="min-width:200px">

<p><a class="all" href="javascript:show('')">All hints</a></p>
<ul>
<li><a id="hint0" href="javascript:show('hint0')">Error: Eta reduce (13)</a></li>
<li><a id="hint1" href="javascript:show('hint1')">Error: Evaluate (10)</a></li>
<li><a id="hint2" href="javascript:show('hint2')">Error: Redundant bracket (17)</a></li>
<li><a id="hint3" href="javascript:show('hint3')">Error: Redundant do (5)</a></li>
<li><a id="hint4" href="javascript:show('hint4')">Error: Redundant if (4)</a></li>
<li><a id="hint5" href="javascript:show('hint5')">Error: Redundant return (4)</a></li>
<li><a id="hint6" href="javascript:show('hint6')">Error: Unused LANGUAGE pragma (7)</a></li>
<li><a id="hint7" href="javascript:show('hint7')">Error: Use . (1)</a></li>
<li><a id="hint8" href="javascript:show('hint8')">Error: Use concatMap (3)</a></li>
<li><a id="hint9" href="javascript:show('hint9')">Error: Use elem (1)</a></li>
<li><a id="hint10" href="javascript:show('hint10')">Error: Use fewer imports (4)</a></li>
<li><a id="hint11" href="javascript:show('hint11')">Error: Use isNothing (1)</a></li>
<li><a id="hint12" href="javascript:show('hint12')">Error: Use print (1)</a></li>
<li><a id="hint13" href="javascript:show('hint13')">Error: Use replicateM (2)</a></li>
<li><a id="hint14" href="javascript:show('hint14')">Warning: Avoid lambda (2)</a></li>
<li><a id="hint15" href="javascript:show('hint15')">Warning: Collapse lambdas (4)</a></li>
<li><a id="hint16" href="javascript:show('hint16')">Warning: Parse error (2)</a></li>
<li><a id="hint17" href="javascript:show('hint17')">Warning: Reduce duplication (6)</a></li>
<li><a id="hint18" href="javascript:show('hint18')">Warning: Redundant $ (50)</a></li>
<li><a id="hint19" href="javascript:show('hint19')">Warning: Redundant as (1)</a></li>
<li><a id="hint20" href="javascript:show('hint20')">Warning: Redundant bracket (87)</a></li>
<li><a id="hint21" href="javascript:show('hint21')">Warning: Redundant guard (1)</a></li>
<li><a id="hint22" href="javascript:show('hint22')">Warning: Use : (2)</a></li>
<li><a id="hint23" href="javascript:show('hint23')">Warning: Use camelCase (17)</a></li>
<li><a id="hint24" href="javascript:show('hint24')">Warning: Use const (1)</a></li>
<li><a id="hint25" href="javascript:show('hint25')">Warning: Use fromMaybe (2)</a></li>
<li><a id="hint26" href="javascript:show('hint26')">Warning: Use guards (2)</a></li>
<li><a id="hint27" href="javascript:show('hint27')">Warning: Use if (3)</a></li>
<li><a id="hint28" href="javascript:show('hint28')">Warning: Use map once (3)</a></li>
<li><a id="hint29" href="javascript:show('hint29')">Warning: Use record patterns (4)</a></li>
<li><a id="hint30" href="javascript:show('hint30')">Warning: Use string literal (1)</a></li>
<li><a id="hint31" href="javascript:show('hint31')">Warning: Use uncurry (1)</a></li>
</ul>

<p><a class="all" href="javascript:show('')">All files</a></p>
<ul>
<li><a id="file0" href="javascript:show('file0')">src/Strappy/BooleanCircuits.hs (13)</a></li>
<li><a id="file1" href="javascript:show('file1')">src/Strappy/CombMap.hs (1)</a></li>
<li><a id="file2" href="javascript:show('file2')">src/Strappy/CombSequitur.hs (3)</a></li>
<li><a id="file3" href="javascript:show('file3')">src/Strappy/Compress.hs (8)</a></li>
<li><a id="file4" href="javascript:show('file4')">src/Strappy/CompressionSearch.hs (4)</a></li>
<li><a id="file5" href="javascript:show('file5')">src/Strappy/DigArith.hs (16)</a></li>
<li><a id="file6" href="javascript:show('file6')">src/Strappy/DigIncr.hs (3)</a></li>
<li><a id="file7" href="javascript:show('file7')">src/Strappy/EnumBF.hs (16)</a></li>
<li><a id="file8" href="javascript:show('file8')">src/Strappy/EnumWoMemo.hs (1)</a></li>
<li><a id="file9" href="javascript:show('file9')">src/Strappy/Enumerate.hs (4)</a></li>
<li><a id="file10" href="javascript:show('file10')">src/Strappy/Experiment.hs (1)</a></li>
<li><a id="file11" href="javascript:show('file11')">src/Strappy/Expr.hs (4)</a></li>
<li><a id="file12" href="javascript:show('file12')">src/Strappy/Grammar.hs (15)</a></li>
<li><a id="file13" href="javascript:show('file13')">src/Strappy/GraphSearch.hs (7)</a></li>
<li><a id="file14" href="javascript:show('file14')">src/Strappy/ParseCL.hs (3)</a></li>
<li><a id="file15" href="javascript:show('file15')">src/Strappy/PostProcess.hs (24)</a></li>
<li><a id="file16" href="javascript:show('file16')">src/Strappy/Routers.hs (8)</a></li>
<li><a id="file17" href="javascript:show('file17')">src/Strappy/Run.hs (3)</a></li>
<li><a id="file18" href="javascript:show('file18')">src/Strappy/Sample.hs (2)</a></li>
<li><a id="file19" href="javascript:show('file19')">src/Strappy/Search.hs (1)</a></li>
<li><a id="file20" href="javascript:show('file20')">src/Strappy/SimpleCL.hs (3)</a></li>
<li><a id="file21" href="javascript:show('file21')">src/Strappy/StdLib.hs (26)</a></li>
<li><a id="file22" href="javascript:show('file22')">src/Strappy/Task.hs (3)</a></li>
<li><a id="file23" href="javascript:show('file23')">src/Strappy/Test.hs (4)</a></li>
<li><a id="file24" href="javascript:show('file24')">src/Strappy/Tests/AmbTI.hs (5)</a></li>
<li><a id="file25" href="javascript:show('file25')">src/Strappy/Tests/ListState.hs (4)</a></li>
<li><a id="file26" href="javascript:show('file26')">src/Strappy/Tests/TestBestFirst.hs (1)</a></li>
<li><a id="file27" href="javascript:show('file27')">src/Strappy/Tests/TestBranching.hs (1)</a></li>
<li><a id="file28" href="javascript:show('file28')">src/Strappy/Tests/TestCompress.hs (1)</a></li>
<li><a id="file29" href="javascript:show('file29')">src/Strappy/Tests/TestCompressionSearch.hs (5)</a></li>
<li><a id="file30" href="javascript:show('file30')">src/Strappy/Tests/TestEnum.hs (2)</a></li>
<li><a id="file31" href="javascript:show('file31')">src/Strappy/Tests/TestGrammar.hs (8)</a></li>
<li><a id="file32" href="javascript:show('file32')">src/Strappy/Tests/TestHashMap.hs (3)</a></li>
<li><a id="file33" href="javascript:show('file33')">src/Strappy/Tests/TestListTI.hs (15)</a></li>
<li><a id="file34" href="javascript:show('file34')">src/Strappy/Tests/TestStateT.hs (8)</a></li>
<li><a id="file35" href="javascript:show('file35')">src/Strappy/Tests/TestTrans.hs (3)</a></li>
<li><a id="file36" href="javascript:show('file36')">src/Strappy/Tests/TestTrans2.hs (5)</a></li>
<li><a id="file37" href="javascript:show('file37')">src/Strappy/Tests/TestType.hs (9)</a></li>
<li><a id="file38" href="javascript:show('file38')">src/Strappy/Tests/TestUtils.hs (1)</a></li>
<li><a id="file39" href="javascript:show('file39')">src/Strappy/Tests/TestingLogict.hs (3)</a></li>
<li><a id="file40" href="javascript:show('file40')">src/Strappy/Type.hs (10)</a></li>
<li><a id="file41" href="javascript:show('file41')">src/Strappy/Visualize.hs (5)</a></li>
</ul>

</div>
<div id="content" valign="top" width="100%">
<p>
	Report generated by <a href="http://community.haskell.org/~ndm/hlint/">HLint</a>
v1.8.37
	- a tool to suggest improvements to your Haskell code.
</p>

<div class="hint17 file0">
src/Strappy/BooleanCircuits.hs:318:12: Warning: Reduce duplication<br/>
Found<br/>
<pre><span class='hs-definition'>inId1</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>read</span> <span class='hs-varid'>inId1str</span>
<span class='hs-definition'>inId2</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>read</span> <span class='hs-varid'>inId2str</span>
<span class='hs-definition'>outId</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>read</span> <span class='hs-varid'>outIdstr</span>
</pre>
Why not<br/>
<pre><span class='hs-conid'>Combine</span> <span class='hs-varid'>with</span> <span class='hs-varid'>src</span><span class='hs-varop'>/</span><span class='hs-conid'>Strappy</span><span class='hs-varop'>/</span><span class='hs-conid'>BooleanCircuits</span><span class='hs-varop'>.</span><span class='hs-varid'>hs</span><span class='hs-conop'>:</span><span class='hs-num'>322</span><span class='hs-conop'>:</span><span class='hs-num'>11</span></pre>

</div>

<div class="hint18 file0">
src/Strappy/BooleanCircuits.hs:62:25: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>maximum</span> <span class='hs-varop'>$</span> <span class='hs-varid'>map</span> <span class='hs-varid'>gateId</span> <span class='hs-varid'>gs</span><span class='hs-layout'>)</span> <span class='hs-varop'>+</span> <span class='hs-num'>1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>maximum</span> <span class='hs-layout'>(</span><span class='hs-varid'>map</span> <span class='hs-varid'>gateId</span> <span class='hs-varid'>gs</span><span class='hs-layout'>)</span> <span class='hs-varop'>+</span> <span class='hs-num'>1</span></pre>

</div>

<div class="hint20 file0">
src/Strappy/BooleanCircuits.hs:71:33: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>ConstInst</span> <span class='hs-varid'>id</span> <span class='hs-layout'>(</span><span class='hs-conid'>Const</span> <span class='hs-varid'>val</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>gs</span></pre>
Why not<br/>
<pre><span class='hs-conid'>ConstInst</span> <span class='hs-varid'>id</span> <span class='hs-layout'>(</span><span class='hs-conid'>Const</span> <span class='hs-varid'>val</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>gs</span></pre>

</div>

<div class="hint20 file0">
src/Strappy/BooleanCircuits.hs:85:19: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>monoGateFunc</span> <span class='hs-varid'>g</span><span class='hs-layout'>)</span> <span class='hs-varid'>inpVal</span></pre>
Why not<br/>
<pre><span class='hs-definition'>monoGateFunc</span> <span class='hs-varid'>g</span> <span class='hs-varid'>inpVal</span></pre>

</div>

<div class="hint20 file0">
src/Strappy/BooleanCircuits.hs:91:19: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>binGateFunc</span> <span class='hs-varid'>g</span><span class='hs-layout'>)</span> <span class='hs-varid'>inpVal1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>binGateFunc</span> <span class='hs-varid'>g</span> <span class='hs-varid'>inpVal1</span></pre>

</div>

<div class="hint13 file0">
src/Strappy/BooleanCircuits.hs:138:38: Error: Use replicateM<br/>
Found<br/>
<pre><span class='hs-definition'>sequence</span> <span class='hs-varop'>$</span> <span class='hs-varid'>replicate</span> <span class='hs-varid'>n</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromList</span> <span class='hs-varid'>dist</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>replicateM</span> <span class='hs-varid'>n</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromList</span> <span class='hs-varid'>dist</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint0 file0">
src/Strappy/BooleanCircuits.hs:160:1: Error: Eta reduce<br/>
Found<br/>
<pre><span class='hs-definition'>addInstances</span> <span class='hs-varid'>circ</span> <span class='hs-varid'>gs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>foldM</span> <span class='hs-varid'>addInst</span> <span class='hs-varid'>circ</span> <span class='hs-varid'>gs</span></pre>
Why not<br/>
<pre><span class='hs-definition'>addInstances</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>foldM</span> <span class='hs-varid'>addInst</span></pre>

</div>

<div class="hint20 file0">
src/Strappy/BooleanCircuits.hs:163:26: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-conid'>ConstInst</span> <span class='hs-varid'>i</span> <span class='hs-varid'>true</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>i</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-keyglyph'>[</span><span class='hs-num'>0</span> <span class='hs-keyglyph'>..</span> <span class='hs-layout'>(</span><span class='hs-varid'>n</span> <span class='hs-comment'>-</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span></pre>
Why not<br/>
<pre><span class='hs-keyglyph'>[</span><span class='hs-conid'>ConstInst</span> <span class='hs-varid'>i</span> <span class='hs-varid'>true</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>i</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-keyglyph'>[</span><span class='hs-num'>0</span> <span class='hs-keyglyph'>..</span> <span class='hs-layout'>(</span><span class='hs-varid'>n</span> <span class='hs-comment'>-</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span></pre>

</div>

<div class="hint13 file0">
src/Strappy/BooleanCircuits.hs:216:39: Error: Use replicateM<br/>
Found<br/>
<pre><span class='hs-definition'>sequence</span> <span class='hs-varop'>$</span> <span class='hs-varid'>replicate</span> <span class='hs-varid'>nC</span> <span class='hs-varop'>$</span> <span class='hs-varid'>sampleConnectedCircuit</span> <span class='hs-varid'>dI</span> <span class='hs-varid'>dN</span> <span class='hs-varid'>dG</span></pre>
Why not<br/>
<pre><span class='hs-definition'>replicateM</span> <span class='hs-varid'>nC</span> <span class='hs-layout'>(</span><span class='hs-varid'>sampleConnectedCircuit</span> <span class='hs-varid'>dI</span> <span class='hs-varid'>dN</span> <span class='hs-varid'>dG</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint0 file0">
src/Strappy/BooleanCircuits.hs:243:1: Error: Eta reduce<br/>
Found<br/>
<pre><span class='hs-definition'>mkEquivClasses</span> <span class='hs-varid'>cs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>foldl</span> <span class='hs-varid'>mkEquivClasses'</span> <span class='hs-conid'>[]</span> <span class='hs-varid'>cs</span></pre>
Why not<br/>
<pre><span class='hs-definition'>mkEquivClasses</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>foldl</span> <span class='hs-varid'>mkEquivClasses'</span> <span class='hs-conid'>[]</span></pre>

</div>

<div class="hint20 file0">
src/Strappy/BooleanCircuits.hs:247:33: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>head</span> <span class='hs-varid'>cl</span><span class='hs-layout'>)</span> <span class='hs-varop'>`equiv`</span> <span class='hs-varid'>c</span></pre>
Why not<br/>
<pre><span class='hs-definition'>head</span> <span class='hs-varid'>cl</span> <span class='hs-varop'>`equiv`</span> <span class='hs-varid'>c</span></pre>

</div>

<div class="hint2 file0">
src/Strappy/BooleanCircuits.hs:249:34: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>cl</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>cl</span></pre>

</div>

<div class="hint20 file0">
src/Strappy/BooleanCircuits.hs:249:34: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>cl</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-layout'>(</span><span class='hs-varid'>mkEquivClasses'</span> <span class='hs-varid'>cls</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>cl</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>mkEquivClasses'</span> <span class='hs-varid'>cls</span> <span class='hs-varid'>c</span></pre>

</div>

<div class="hint19 file1">
src/Strappy/CombMap.hs:98:1: Warning: Redundant as<br/>
Found<br/>
<pre><span class='hs-keyword'>import</span> <span class='hs-keyword'>qualified</span> <span class='hs-conid'>Prelude</span> <span class='hs-keyword'>as</span> <span class='hs-conid'>Prelude</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>import</span> <span class='hs-keyword'>qualified</span> <span class='hs-conid'>Prelude</span></pre>

</div>

<div class="hint22 file2">
src/Strappy/CombSequitur.hs:18:23: Warning: Use :<br/>
Found<br/>
<pre><span class='hs-str'>"c"</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>i</span></pre>
Why not<br/>
<pre><span class='hs-chr'>'c'</span> <span class='hs-conop'>:</span> <span class='hs-varid'>show</span> <span class='hs-varid'>i</span></pre>

</div>

<div class="hint29 file2">
src/Strappy/CombSequitur.hs:20:17: Warning: Use record patterns<br/>
Found<br/>
<pre><span class='hs-conid'>CLeaf</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span></pre>
Why not<br/>
<pre><span class='hs-conid'>CLeaf</span><span class='hs-layout'>{</span><span class='hs-layout'>}</span></pre>

</div>

<div class="hint27 file2">
src/Strappy/CombSequitur.hs:58:40: Warning: Use if<br/>
Found<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-varid'>s</span> <span class='hs-varop'>`</span><span class='hs-conid'>Map</span><span class='hs-varop'>.</span><span class='hs-varid'>member</span><span class='hs-varop'>`</span> <span class='hs-varid'>index</span> <span class='hs-keyword'>of</span>
    <span class='hs-conid'>True</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span> <span class='hs-keyword'>let</span> <span class='hs-varid'>r</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-varid'>newSymbol</span> <span class='hs-varid'>g</span><span class='hs-layout'>,</span> <span class='hs-varid'>s</span><span class='hs-layout'>)</span>
               <span class='hs-varid'>putGrammar</span> <span class='hs-layout'>(</span><span class='hs-varid'>r</span> <span class='hs-conop'>:</span> <span class='hs-varid'>g</span><span class='hs-layout'>)</span>
    <span class='hs-conid'>False</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>undefined</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>if</span> <span class='hs-varid'>s</span> <span class='hs-varop'>`</span><span class='hs-conid'>Map</span><span class='hs-varop'>.</span><span class='hs-varid'>member</span><span class='hs-varop'>`</span> <span class='hs-varid'>index</span> <span class='hs-keyword'>then</span>
  <span class='hs-layout'>(</span><span class='hs-keyword'>do</span> <span class='hs-keyword'>let</span> <span class='hs-varid'>r</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-varid'>newSymbol</span> <span class='hs-varid'>g</span><span class='hs-layout'>,</span> <span class='hs-varid'>s</span><span class='hs-layout'>)</span>
      <span class='hs-varid'>putGrammar</span> <span class='hs-layout'>(</span><span class='hs-varid'>r</span> <span class='hs-conop'>:</span> <span class='hs-varid'>g</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
  <span class='hs-keyword'>else</span> <span class='hs-varid'>undefined</span></pre>

</div>

<div class="hint17 file3">
src/Strappy/Compress.hs:57:26: Warning: Reduce duplication<br/>
Found<br/>
<pre><span class='hs-definition'>l_index</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>incr2</span> <span class='hs-varid'>index'</span> <span class='hs-varid'>l</span> <span class='hs-varid'>t_left</span>
<span class='hs-definition'>r_index</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>incr2</span> <span class='hs-varid'>l_index</span> <span class='hs-varid'>r</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromType</span> <span class='hs-layout'>(</span><span class='hs-varid'>cType</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
<span class='hs-definition'>return</span> <span class='hs-varid'>r_index</span>
</pre>
Why not<br/>
<pre><span class='hs-conid'>Combine</span> <span class='hs-varid'>with</span> <span class='hs-varid'>src</span><span class='hs-varop'>/</span><span class='hs-conid'>Strappy</span><span class='hs-varop'>/</span><span class='hs-conid'>Compress</span><span class='hs-varop'>.</span><span class='hs-varid'>hs</span><span class='hs-conop'>:</span><span class='hs-num'>61</span><span class='hs-conop'>:</span><span class='hs-num'>30</span></pre>

</div>

<div class="hint29 file3">
src/Strappy/Compress.hs:41:15: Warning: Use record patterns<br/>
Found<br/>
<pre><span class='hs-conid'>CLeaf</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span></pre>
Why not<br/>
<pre><span class='hs-conid'>CLeaf</span><span class='hs-layout'>{</span><span class='hs-layout'>}</span></pre>

</div>

<div class="hint0 file3">
src/Strappy/Compress.hs:47:1: Error: Eta reduce<br/>
Found<br/>
<pre><span class='hs-definition'>compress</span> <span class='hs-varid'>cs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>foldl'</span> <span class='hs-varid'>incr</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>empty</span> <span class='hs-varid'>cs</span></pre>
Why not<br/>
<pre><span class='hs-definition'>compress</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>foldl'</span> <span class='hs-varid'>incr</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>empty</span></pre>

</div>

<div class="hint5 file3">
src/Strappy/Compress.hs:56:23: Error: Redundant return<br/>
Found<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-keyword'>let</span> <span class='hs-varid'>index'</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>insert</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>tp</span><span class='hs-keyglyph'>]</span> <span class='hs-varid'>index</span>
   <span class='hs-varid'>l_index</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>incr2</span> <span class='hs-varid'>index'</span> <span class='hs-varid'>l</span> <span class='hs-varid'>t_left</span>
   <span class='hs-varid'>r_index</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>incr2</span> <span class='hs-varid'>l_index</span> <span class='hs-varid'>r</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromType</span> <span class='hs-layout'>(</span><span class='hs-varid'>cType</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
   <span class='hs-varid'>return</span> <span class='hs-varid'>r_index</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-keyword'>let</span> <span class='hs-varid'>index'</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>insert</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>tp</span><span class='hs-keyglyph'>]</span> <span class='hs-varid'>index</span>
   <span class='hs-varid'>l_index</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>incr2</span> <span class='hs-varid'>index'</span> <span class='hs-varid'>l</span> <span class='hs-varid'>t_left</span>
   <span class='hs-varid'>incr2</span> <span class='hs-varid'>l_index</span> <span class='hs-varid'>r</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromType</span> <span class='hs-layout'>(</span><span class='hs-varid'>cType</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint5 file3">
src/Strappy/Compress.hs:60:27: Error: Redundant return<br/>
Found<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-keyword'>let</span> <span class='hs-varid'>index'</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>insert</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>tp</span><span class='hs-layout'>,</span> <span class='hs-varid'>x</span><span class='hs-keyglyph'>]</span> <span class='hs-varid'>index</span>
   <span class='hs-varid'>l_index</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>incr2</span> <span class='hs-varid'>index'</span> <span class='hs-varid'>l</span> <span class='hs-varid'>t_left</span>
   <span class='hs-varid'>r_index</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>incr2</span> <span class='hs-varid'>l_index</span> <span class='hs-varid'>r</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromType</span> <span class='hs-layout'>(</span><span class='hs-varid'>cType</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
   <span class='hs-varid'>return</span> <span class='hs-varid'>r_index</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-keyword'>let</span> <span class='hs-varid'>index'</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>insert</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>tp</span><span class='hs-layout'>,</span> <span class='hs-varid'>x</span><span class='hs-keyglyph'>]</span> <span class='hs-varid'>index</span>
   <span class='hs-varid'>l_index</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>incr2</span> <span class='hs-varid'>index'</span> <span class='hs-varid'>l</span> <span class='hs-varid'>t_left</span>
   <span class='hs-varid'>incr2</span> <span class='hs-varid'>l_index</span> <span class='hs-varid'>r</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromType</span> <span class='hs-layout'>(</span><span class='hs-varid'>cType</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint29 file3">
src/Strappy/Compress.hs:66:16: Warning: Use record patterns<br/>
Found<br/>
<pre><span class='hs-conid'>CLeaf</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span></pre>
Why not<br/>
<pre><span class='hs-conid'>CLeaf</span><span class='hs-layout'>{</span><span class='hs-layout'>}</span></pre>

</div>

<div class="hint0 file3">
src/Strappy/Compress.hs:69:1: Error: Eta reduce<br/>
Found<br/>
<pre><span class='hs-definition'>compress2</span> <span class='hs-varid'>xs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>foldl'</span> <span class='hs-varid'>f</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>empty</span> <span class='hs-varid'>xs</span></pre>
Why not<br/>
<pre><span class='hs-definition'>compress2</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>foldl'</span> <span class='hs-varid'>f</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>empty</span></pre>

</div>

<div class="hint29 file3">
src/Strappy/Compress.hs:83:19: Warning: Use record patterns<br/>
Found<br/>
<pre><span class='hs-conid'>CLeaf</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span></pre>
Why not<br/>
<pre><span class='hs-conid'>CLeaf</span><span class='hs-layout'>{</span><span class='hs-layout'>}</span></pre>

</div>

<div class="hint20 file4">
src/Strappy/CompressionSearch.hs:46:15: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>length</span> <span class='hs-varop'>.</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>keys</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>length</span> <span class='hs-varop'>.</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>keys</span></pre>

</div>

<div class="hint20 file4">
src/Strappy/CompressionSearch.hs:59:15: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>length</span> <span class='hs-varop'>.</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>keys</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>length</span> <span class='hs-varop'>.</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>keys</span></pre>

</div>

<div class="hint20 file4">
src/Strappy/CompressionSearch.hs:65:17: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>take</span> <span class='hs-varid'>n</span> <span class='hs-varop'>.</span> <span class='hs-varid'>reverse</span> <span class='hs-varop'>.</span> <span class='hs-varid'>assignment</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>take</span> <span class='hs-varid'>n</span> <span class='hs-varop'>.</span> <span class='hs-varid'>reverse</span> <span class='hs-varop'>.</span> <span class='hs-varid'>assignment</span></pre>

</div>

<div class="hint0 file4">
src/Strappy/CompressionSearch.hs:85:1: Error: Eta reduce<br/>
Found<br/>
<pre><span class='hs-definition'>greedy</span> <span class='hs-varid'>lib</span> <span class='hs-varid'>xs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>foldl'</span> <span class='hs-varid'>g</span> <span class='hs-layout'>(</span><span class='hs-varid'>lib</span><span class='hs-layout'>,</span> <span class='hs-conid'>[]</span><span class='hs-layout'>)</span> <span class='hs-varid'>xs</span></pre>
Why not<br/>
<pre><span class='hs-definition'>greedy</span> <span class='hs-varid'>lib</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>foldl'</span> <span class='hs-varid'>g</span> <span class='hs-layout'>(</span><span class='hs-varid'>lib</span><span class='hs-layout'>,</span> <span class='hs-conid'>[]</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint10 file5">
src/Strappy/DigArith.hs:13:1: Error: Use fewer imports<br/>
Found<br/>
<pre><span class='hs-keyword'>import</span> <span class='hs-conid'>Strappy</span><span class='hs-varop'>.</span><span class='hs-conid'>Type</span>
<span class='hs-keyword'>import</span> <span class='hs-conid'>Strappy</span><span class='hs-varop'>.</span><span class='hs-conid'>Type</span>
</pre>
Why not<br/>
<pre><span class='hs-keyword'>import</span> <span class='hs-conid'>Strappy</span><span class='hs-varop'>.</span><span class='hs-conid'>Type</span>
</pre>

</div>

<div class="hint17 file5">
src/Strappy/DigArith.hs:34:19: Warning: Reduce duplication<br/>
Found<br/>
<pre><span class='hs-definition'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>n</span> <span class='hs-varop'>`div`</span> <span class='hs-num'>1</span> <span class='hs-varop'>`mod`</span> <span class='hs-num'>2</span>
<span class='hs-definition'>b</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-varid'>n</span> <span class='hs-comment'>-</span> <span class='hs-varid'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span> <span class='hs-varop'>`div`</span> <span class='hs-num'>2</span> <span class='hs-varop'>`mod`</span> <span class='hs-num'>2</span>
<span class='hs-definition'>c</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-varid'>n</span> <span class='hs-comment'>-</span> <span class='hs-varid'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span> <span class='hs-comment'>-</span> <span class='hs-varid'>b</span> <span class='hs-varop'>*</span> <span class='hs-num'>2</span><span class='hs-layout'>)</span> <span class='hs-varop'>`div`</span> <span class='hs-num'>4</span> <span class='hs-varop'>`mod`</span> <span class='hs-num'>2</span>
</pre>
Why not<br/>
<pre><span class='hs-conid'>Combine</span> <span class='hs-varid'>with</span> <span class='hs-varid'>src</span><span class='hs-varop'>/</span><span class='hs-conid'>Strappy</span><span class='hs-varop'>/</span><span class='hs-conid'>DigArith</span><span class='hs-varop'>.</span><span class='hs-varid'>hs</span><span class='hs-conop'>:</span><span class='hs-num'>38</span><span class='hs-conop'>:</span><span class='hs-num'>19</span></pre>

</div>

<div class="hint17 file5">
src/Strappy/DigArith.hs:38:19: Warning: Reduce duplication<br/>
Found<br/>
<pre><span class='hs-definition'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>n</span> <span class='hs-varop'>`div`</span> <span class='hs-num'>1</span> <span class='hs-varop'>`mod`</span> <span class='hs-num'>2</span>
<span class='hs-definition'>b</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-varid'>n</span> <span class='hs-comment'>-</span> <span class='hs-varid'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span> <span class='hs-varop'>`div`</span> <span class='hs-num'>2</span> <span class='hs-varop'>`mod`</span> <span class='hs-num'>2</span>
<span class='hs-definition'>c</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-varid'>n</span> <span class='hs-comment'>-</span> <span class='hs-varid'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span> <span class='hs-comment'>-</span> <span class='hs-varid'>b</span> <span class='hs-varop'>*</span> <span class='hs-num'>2</span><span class='hs-layout'>)</span> <span class='hs-varop'>`div`</span> <span class='hs-num'>4</span> <span class='hs-varop'>`mod`</span> <span class='hs-num'>2</span>
<span class='hs-definition'>d</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-varid'>n</span> <span class='hs-comment'>-</span> <span class='hs-varid'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span> <span class='hs-comment'>-</span> <span class='hs-varid'>b</span> <span class='hs-varop'>*</span> <span class='hs-num'>2</span> <span class='hs-comment'>-</span> <span class='hs-varid'>c</span> <span class='hs-varop'>*</span> <span class='hs-num'>4</span><span class='hs-layout'>)</span> <span class='hs-varop'>`div`</span> <span class='hs-num'>16</span> <span class='hs-varop'>`mod`</span> <span class='hs-num'>2</span>
</pre>
Why not<br/>
<pre><span class='hs-conid'>Combine</span> <span class='hs-varid'>with</span> <span class='hs-varid'>src</span><span class='hs-varop'>/</span><span class='hs-conid'>Strappy</span><span class='hs-varop'>/</span><span class='hs-conid'>DigArith</span><span class='hs-varop'>.</span><span class='hs-varid'>hs</span><span class='hs-conop'>:</span><span class='hs-num'>43</span><span class='hs-conop'>:</span><span class='hs-num'>19</span></pre>

</div>

<div class="hint1 file5">
src/Strappy/DigArith.hs:32:26: Error: Evaluate<br/>
Found<br/>
<pre><span class='hs-definition'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>a</span></pre>

</div>

<div class="hint1 file5">
src/Strappy/DigArith.hs:35:26: Error: Evaluate<br/>
Found<br/>
<pre><span class='hs-definition'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>a</span></pre>

</div>

<div class="hint1 file5">
src/Strappy/DigArith.hs:36:26: Error: Evaluate<br/>
Found<br/>
<pre><span class='hs-definition'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>a</span></pre>

</div>

<div class="hint1 file5">
src/Strappy/DigArith.hs:39:26: Error: Evaluate<br/>
Found<br/>
<pre><span class='hs-definition'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>a</span></pre>

</div>

<div class="hint1 file5">
src/Strappy/DigArith.hs:40:26: Error: Evaluate<br/>
Found<br/>
<pre><span class='hs-definition'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>a</span></pre>

</div>

<div class="hint1 file5">
src/Strappy/DigArith.hs:41:26: Error: Evaluate<br/>
Found<br/>
<pre><span class='hs-definition'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>a</span></pre>

</div>

<div class="hint1 file5">
src/Strappy/DigArith.hs:44:26: Error: Evaluate<br/>
Found<br/>
<pre><span class='hs-definition'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>a</span></pre>

</div>

<div class="hint1 file5">
src/Strappy/DigArith.hs:45:26: Error: Evaluate<br/>
Found<br/>
<pre><span class='hs-definition'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>a</span></pre>

</div>

<div class="hint1 file5">
src/Strappy/DigArith.hs:46:26: Error: Evaluate<br/>
Found<br/>
<pre><span class='hs-definition'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>a</span></pre>

</div>

<div class="hint1 file5">
src/Strappy/DigArith.hs:47:26: Error: Evaluate<br/>
Found<br/>
<pre><span class='hs-definition'>a</span> <span class='hs-varop'>*</span> <span class='hs-num'>1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>a</span></pre>

</div>

<div class="hint27 file5">
src/Strappy/DigArith.hs:79:8: Warning: Use if<br/>
Found<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-varid'>all</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varop'>==</span> <span class='hs-varid'>card</span><span class='hs-layout'>)</span> <span class='hs-varop'>.</span> <span class='hs-varid'>length</span> <span class='hs-varop'>.</span> <span class='hs-varid'>fst</span><span class='hs-layout'>)</span> <span class='hs-varid'>xs</span> <span class='hs-keyword'>of</span>
    <span class='hs-conid'>True</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>card</span>
    <span class='hs-conid'>False</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>error</span> <span class='hs-varop'>$</span>
               <span class='hs-str'>"in truthTableCardinality: "</span> <span class='hs-varop'>++</span>
                 <span class='hs-str'>"all assignments are not of equal length."</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-keyword'>if</span> <span class='hs-varid'>all</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varop'>==</span> <span class='hs-varid'>card</span><span class='hs-layout'>)</span> <span class='hs-varop'>.</span> <span class='hs-varid'>length</span> <span class='hs-varop'>.</span> <span class='hs-varid'>fst</span><span class='hs-layout'>)</span> <span class='hs-varid'>xs</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>card</span> <span class='hs-keyword'>else</span>
   <span class='hs-varid'>error</span> <span class='hs-varop'>$</span>
     <span class='hs-str'>"in truthTableCardinality: "</span> <span class='hs-varop'>++</span>
       <span class='hs-str'>"all assignments are not of equal length."</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint14 file5">
src/Strappy/DigArith.hs:113:34: Warning: Avoid lambda<br/>
Found<br/>
<pre><span class='hs-keyglyph'>\</span> <span class='hs-varid'>x</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>app'</span> <span class='hs-varid'>x</span> <span class='hs-varid'>v</span></pre>
Why not<br/>
<pre><span class='hs-definition'>app'</span></pre>

</div>

<div class="hint18 file5">
src/Strappy/DigArith.hs:126:30: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>unwords</span> <span class='hs-varop'>$</span> <span class='hs-varid'>map</span> <span class='hs-varid'>show</span> <span class='hs-varid'>tup</span><span class='hs-layout'>)</span> <span class='hs-varop'>++</span> <span class='hs-str'>" --&gt; "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>v</span></pre>
Why not<br/>
<pre><span class='hs-definition'>unwords</span> <span class='hs-layout'>(</span><span class='hs-varid'>map</span> <span class='hs-varid'>show</span> <span class='hs-varid'>tup</span><span class='hs-layout'>)</span> <span class='hs-varop'>++</span> <span class='hs-str'>" --&gt; "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>v</span></pre>

</div>

<div class="hint6 file6">
src/Strappy/DigIncr.hs:3:1: Error: Unused LANGUAGE pragma<br/>
Found<br/>
<pre><span class='hs-comment'>{-# LANGUAGE ParallelListComp #-}</span></pre>
Why not remove it.<br/>
<pre></pre>

</div>

<div class="hint10 file6">
src/Strappy/DigIncr.hs:15:1: Error: Use fewer imports<br/>
Found<br/>
<pre><span class='hs-keyword'>import</span> <span class='hs-conid'>Type</span>
<span class='hs-keyword'>import</span> <span class='hs-conid'>Type</span>
</pre>
Why not<br/>
<pre><span class='hs-keyword'>import</span> <span class='hs-conid'>Type</span>
</pre>

</div>

<div class="hint18 file6">
src/Strappy/DigIncr.hs:42:11: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-varop'>$</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-str'>"pair"</span><span class='hs-layout'>,</span> <span class='hs-varid'>cPair</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-str'>"fst"</span><span class='hs-layout'>,</span> <span class='hs-varid'>cFst</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-str'>"snd"</span><span class='hs-layout'>,</span> <span class='hs-varid'>cSnd</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span></pre>
Why not<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-str'>"pair"</span><span class='hs-layout'>,</span> <span class='hs-varid'>cPair</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-str'>"fst"</span><span class='hs-layout'>,</span> <span class='hs-varid'>cFst</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-str'>"snd"</span><span class='hs-layout'>,</span> <span class='hs-varid'>cSnd</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span></pre>

</div>

<div class="hint11 file7">
src/Strappy/EnumBF.hs:48:15: Error: Use isNothing<br/>
Found<br/>
<pre><span class='hs-definition'>path</span> <span class='hs-varid'>cb</span> <span class='hs-varop'>==</span> <span class='hs-conid'>Nothing</span></pre>
Why not<br/>
<pre><span class='hs-definition'>isNothing</span> <span class='hs-layout'>(</span><span class='hs-varid'>path</span> <span class='hs-varid'>cb</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint2 file7">
src/Strappy/EnumBF.hs:52:25: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-num'>0.25</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-num'>0.25</span></pre>

</div>

<div class="hint31 file7">
src/Strappy/EnumBF.hs:74:25: Warning: Use uncurry<br/>
Found<br/>
<pre><span class='hs-keyglyph'>\</span> <span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>CombBaseTuple</span> <span class='hs-varid'>c</span> <span class='hs-varid'>i</span></pre>
Why not<br/>
<pre><span class='hs-definition'>uncurry</span> <span class='hs-conid'>CombBaseTuple</span></pre>

</div>

<div class="hint20 file7">
src/Strappy/EnumBF.hs:78:25: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-varid'>closedCBs</span><span class='hs-layout'>)</span> <span class='hs-varop'>`</span><span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>union</span><span class='hs-varop'>`</span> <span class='hs-varid'>closedPQ</span></pre>
Why not<br/>
<pre><span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-varid'>closedCBs</span> <span class='hs-varop'>`</span><span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>union</span><span class='hs-varop'>`</span> <span class='hs-varid'>closedPQ</span></pre>

</div>

<div class="hint18 file7">
src/Strappy/EnumBF.hs:80:12: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span>
   <span class='hs-str'>"OPEN: "</span> <span class='hs-varop'>++</span>
     <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varop'>$</span> <span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>size</span> <span class='hs-varid'>openPQ''</span><span class='hs-layout'>)</span> <span class='hs-varop'>++</span>
       <span class='hs-str'>" CLOSED: "</span> <span class='hs-varop'>++</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varop'>$</span> <span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>size</span> <span class='hs-varid'>closedPQ'</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
  <span class='hs-varop'>$</span> <span class='hs-varid'>enumBF'</span> <span class='hs-varid'>gr</span> <span class='hs-varid'>i</span> <span class='hs-varop'>$</span> <span class='hs-conid'>BFState</span> <span class='hs-varid'>openPQ''</span> <span class='hs-varid'>closedPQ'</span></pre>
Why not<br/>
<pre><span class='hs-definition'>trace</span>
  <span class='hs-layout'>(</span><span class='hs-str'>"OPEN: "</span> <span class='hs-varop'>++</span>
     <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varop'>$</span> <span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>size</span> <span class='hs-varid'>openPQ''</span><span class='hs-layout'>)</span> <span class='hs-varop'>++</span>
       <span class='hs-str'>" CLOSED: "</span> <span class='hs-varop'>++</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varop'>$</span> <span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>size</span> <span class='hs-varid'>closedPQ'</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
  <span class='hs-varop'>$</span> <span class='hs-varid'>enumBF'</span> <span class='hs-varid'>gr</span> <span class='hs-varid'>i</span> <span class='hs-varop'>$</span> <span class='hs-conid'>BFState</span> <span class='hs-varid'>openPQ''</span> <span class='hs-varid'>closedPQ'</span></pre>

</div>

<div class="hint18 file7">
src/Strappy/EnumBF.hs:80:33: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varop'>$</span> <span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>size</span> <span class='hs-varid'>openPQ''</span><span class='hs-layout'>)</span> <span class='hs-varop'>++</span>
  <span class='hs-str'>" CLOSED: "</span> <span class='hs-varop'>++</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varop'>$</span> <span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>size</span> <span class='hs-varid'>closedPQ'</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>show</span> <span class='hs-layout'>(</span><span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>size</span> <span class='hs-varid'>openPQ''</span><span class='hs-layout'>)</span> <span class='hs-varop'>++</span>
  <span class='hs-str'>" CLOSED: "</span> <span class='hs-varop'>++</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varop'>$</span> <span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>size</span> <span class='hs-varid'>closedPQ'</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint18 file7">
src/Strappy/EnumBF.hs:81:16: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-str'>" CLOSED: "</span> <span class='hs-varop'>++</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varop'>$</span> <span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>size</span> <span class='hs-varid'>closedPQ'</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-str'>" CLOSED: "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-layout'>(</span><span class='hs-conid'>PQ</span><span class='hs-varop'>.</span><span class='hs-varid'>size</span> <span class='hs-varid'>closedPQ'</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file7">
src/Strappy/EnumBF.hs:87:5: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>tp'</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>newTVar</span> <span class='hs-conid'>Star</span>
   <span class='hs-keyword'>let</span> <span class='hs-varid'>t_left</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>tp'</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>tp</span>
       <span class='hs-varid'>t_right</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>tp'</span>
       <span class='hs-varid'>c_left</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-conid'>CInnerNode</span> <span class='hs-varid'>t_left</span><span class='hs-layout'>)</span>
       <span class='hs-varid'>c_right</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-conid'>CInnerNode</span> <span class='hs-varid'>t_right</span><span class='hs-layout'>)</span>
       <span class='hs-varid'>c</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CApp</span> <span class='hs-varid'>c_left</span> <span class='hs-varid'>c_right</span> <span class='hs-varid'>tp</span> <span class='hs-num'>1</span> <span class='hs-conid'>Nothing</span>
       <span class='hs-varid'>minGrammarVal</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>hackMinGrammarVal</span>
       <span class='hs-varid'>cb</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CombBase</span> <span class='hs-varid'>c</span> <span class='hs-layout'>(</span><span class='hs-conid'>Just</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>L</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-num'>2</span> <span class='hs-varop'>*</span> <span class='hs-varid'>minGrammarVal</span><span class='hs-layout'>)</span>
   <span class='hs-varid'>return</span> <span class='hs-varid'>cb</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>tp'</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>newTVar</span> <span class='hs-conid'>Star</span>
   <span class='hs-keyword'>let</span> <span class='hs-varid'>t_left</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>tp'</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>tp</span>
       <span class='hs-varid'>t_right</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>tp'</span>
       <span class='hs-varid'>c_left</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CInnerNode</span> <span class='hs-varid'>t_left</span>
       <span class='hs-varid'>c_right</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-conid'>CInnerNode</span> <span class='hs-varid'>t_right</span><span class='hs-layout'>)</span>
       <span class='hs-varid'>c</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CApp</span> <span class='hs-varid'>c_left</span> <span class='hs-varid'>c_right</span> <span class='hs-varid'>tp</span> <span class='hs-num'>1</span> <span class='hs-conid'>Nothing</span>
       <span class='hs-varid'>minGrammarVal</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>hackMinGrammarVal</span>
       <span class='hs-varid'>cb</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CombBase</span> <span class='hs-varid'>c</span> <span class='hs-layout'>(</span><span class='hs-conid'>Just</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>L</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-num'>2</span> <span class='hs-varop'>*</span> <span class='hs-varid'>minGrammarVal</span><span class='hs-layout'>)</span>
   <span class='hs-varid'>return</span> <span class='hs-varid'>cb</span></pre>

</div>

<div class="hint20 file7">
src/Strappy/EnumBF.hs:87:5: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>tp'</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>newTVar</span> <span class='hs-conid'>Star</span>
   <span class='hs-keyword'>let</span> <span class='hs-varid'>t_left</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>tp'</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>tp</span>
       <span class='hs-varid'>t_right</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>tp'</span>
       <span class='hs-varid'>c_left</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-conid'>CInnerNode</span> <span class='hs-varid'>t_left</span><span class='hs-layout'>)</span>
       <span class='hs-varid'>c_right</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-conid'>CInnerNode</span> <span class='hs-varid'>t_right</span><span class='hs-layout'>)</span>
       <span class='hs-varid'>c</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CApp</span> <span class='hs-varid'>c_left</span> <span class='hs-varid'>c_right</span> <span class='hs-varid'>tp</span> <span class='hs-num'>1</span> <span class='hs-conid'>Nothing</span>
       <span class='hs-varid'>minGrammarVal</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>hackMinGrammarVal</span>
       <span class='hs-varid'>cb</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CombBase</span> <span class='hs-varid'>c</span> <span class='hs-layout'>(</span><span class='hs-conid'>Just</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>L</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-num'>2</span> <span class='hs-varop'>*</span> <span class='hs-varid'>minGrammarVal</span><span class='hs-layout'>)</span>
   <span class='hs-varid'>return</span> <span class='hs-varid'>cb</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>tp'</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>newTVar</span> <span class='hs-conid'>Star</span>
   <span class='hs-keyword'>let</span> <span class='hs-varid'>t_left</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>tp'</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>tp</span>
       <span class='hs-varid'>t_right</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>tp'</span>
       <span class='hs-varid'>c_left</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-conid'>CInnerNode</span> <span class='hs-varid'>t_left</span><span class='hs-layout'>)</span>
       <span class='hs-varid'>c_right</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CInnerNode</span> <span class='hs-varid'>t_right</span>
       <span class='hs-varid'>c</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CApp</span> <span class='hs-varid'>c_left</span> <span class='hs-varid'>c_right</span> <span class='hs-varid'>tp</span> <span class='hs-num'>1</span> <span class='hs-conid'>Nothing</span>
       <span class='hs-varid'>minGrammarVal</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>hackMinGrammarVal</span>
       <span class='hs-varid'>cb</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CombBase</span> <span class='hs-varid'>c</span> <span class='hs-layout'>(</span><span class='hs-conid'>Just</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>L</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-num'>2</span> <span class='hs-varop'>*</span> <span class='hs-varid'>minGrammarVal</span><span class='hs-layout'>)</span>
   <span class='hs-varid'>return</span> <span class='hs-varid'>cb</span></pre>

</div>

<div class="hint18 file7">
src/Strappy/EnumBF.hs:102:27: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-str'>"EMPTY"</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>mzero</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-str'>"EMPTY"</span><span class='hs-layout'>)</span> <span class='hs-varid'>mzero</span></pre>

</div>

<div class="hint18 file7">
src/Strappy/EnumBF.hs:102:28: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>trace</span> <span class='hs-varop'>$</span> <span class='hs-str'>"EMPTY"</span></pre>
Why not<br/>
<pre><span class='hs-definition'>trace</span> <span class='hs-str'>"EMPTY"</span></pre>

</div>

<div class="hint20 file7">
src/Strappy/EnumBF.hs:109:34: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>calcLogProb</span> <span class='hs-varid'>gr</span> <span class='hs-varid'>tp</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-varop'>+</span> <span class='hs-varid'>minGrammarVal</span></pre>
Why not<br/>
<pre><span class='hs-definition'>calcLogProb</span> <span class='hs-varid'>gr</span> <span class='hs-varid'>tp</span> <span class='hs-varid'>c</span> <span class='hs-varop'>+</span> <span class='hs-varid'>minGrammarVal</span></pre>

</div>

<div class="hint4 file7">
src/Strappy/EnumBF.hs:112:26: Error: Redundant if<br/>
Found<br/>
<pre><span class='hs-keyword'>if</span> <span class='hs-varid'>not</span> <span class='hs-varop'>$</span> <span class='hs-varid'>isTAp</span> <span class='hs-varid'>tp</span> <span class='hs-keyword'>then</span> <span class='hs-conid'>False</span> <span class='hs-keyword'>else</span>
  <span class='hs-varid'>not</span> <span class='hs-varop'>$</span> <span class='hs-varid'>existCombWithToType</span> <span class='hs-varid'>primCombs</span> <span class='hs-layout'>(</span><span class='hs-varid'>toType</span> <span class='hs-varid'>tp</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>not</span> <span class='hs-layout'>(</span><span class='hs-varid'>not</span> <span class='hs-varop'>$</span> <span class='hs-varid'>isTAp</span> <span class='hs-varid'>tp</span><span class='hs-layout'>)</span> <span class='hs-varop'>&amp;&amp;</span>
  <span class='hs-layout'>(</span><span class='hs-varid'>not</span> <span class='hs-varop'>$</span> <span class='hs-varid'>existCombWithToType</span> <span class='hs-varid'>primCombs</span> <span class='hs-layout'>(</span><span class='hs-varid'>toType</span> <span class='hs-varid'>tp</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file7">
src/Strappy/EnumBF.hs:131:20: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>cDepth</span> <span class='hs-varid'>c_left'</span><span class='hs-layout'>)</span> <span class='hs-varop'>+</span> <span class='hs-num'>1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>cDepth</span> <span class='hs-varid'>c_left'</span> <span class='hs-varop'>+</span> <span class='hs-num'>1</span></pre>

</div>

<div class="hint20 file7">
src/Strappy/EnumBF.hs:149:20: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>cDepth</span> <span class='hs-varid'>c_right'</span><span class='hs-layout'>)</span> <span class='hs-varop'>+</span> <span class='hs-num'>1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>cDepth</span> <span class='hs-varid'>c_right'</span> <span class='hs-varop'>+</span> <span class='hs-num'>1</span></pre>

</div>

<div class="hint9 file7">
src/Strappy/EnumBF.hs:168:28: Error: Use elem<br/>
Found<br/>
<pre><span class='hs-definition'>any</span> <span class='hs-layout'>(</span><span class='hs-varop'>==</span> <span class='hs-conid'>True</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>elem</span> <span class='hs-conid'>True</span></pre>

</div>

<div class="hint6 file9">
src/Strappy/Enumerate.hs:2:1: Error: Unused LANGUAGE pragma<br/>
Found<br/>
<pre><span class='hs-comment'>{-# LANGUAGE ParallelListComp, BangPatterns #-}</span></pre>
Why not remove it.<br/>
<pre></pre>

</div>

<div class="hint17 file9">
src/Strappy/Enumerate.hs:39:3: Warning: Reduce duplication<br/>
Found<br/>
<pre><span class='hs-keyword'>let</span> <span class='hs-varid'>t_right1</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>cType</span> <span class='hs-varid'>right</span>
<span class='hs-definition'>tp'</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>newTVar</span> <span class='hs-conid'>Star</span>
<span class='hs-keyword'>let</span> <span class='hs-varid'>backsub</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>fromJust</span> <span class='hs-varop'>$</span> <span class='hs-varid'>mgu</span> <span class='hs-varid'>t_left1</span> <span class='hs-layout'>(</span><span class='hs-varid'>t_right1</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>tp'</span><span class='hs-layout'>)</span>
    <span class='hs-varid'>combined</span>
      <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CApp</span> <span class='hs-varid'>left</span> <span class='hs-varid'>right</span> <span class='hs-layout'>(</span><span class='hs-varid'>toType</span> <span class='hs-layout'>(</span><span class='hs-varid'>apply</span> <span class='hs-varid'>backsub</span> <span class='hs-varid'>t_left1</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
          <span class='hs-layout'>(</span><span class='hs-varid'>mkAppDepth</span> <span class='hs-varid'>left</span> <span class='hs-varid'>right</span><span class='hs-layout'>)</span>
          <span class='hs-conid'>Nothing</span>
<span class='hs-definition'>return</span> <span class='hs-varid'>combined</span>
</pre>
Why not<br/>
<pre><span class='hs-conid'>Combine</span> <span class='hs-varid'>with</span> <span class='hs-varid'>src</span><span class='hs-varop'>/</span><span class='hs-conid'>Strappy</span><span class='hs-varop'>/</span><span class='hs-conid'>Enumerate</span><span class='hs-varop'>.</span><span class='hs-varid'>hs</span><span class='hs-conop'>:</span><span class='hs-num'>76</span><span class='hs-conop'>:</span><span class='hs-num'>3</span></pre>

</div>

<div class="hint20 file9">
src/Strappy/Enumerate.hs:56:11: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-conid'>Int</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-conid'>Int</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span>
  <span class='hs-layout'>(</span><span class='hs-conid'>TypeInference</span> <span class='hs-conid'>[]</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>TypeInference</span> <span class='hs-conid'>[]</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-conid'>Int</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-conid'>Int</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span>
  <span class='hs-conid'>TypeInference</span> <span class='hs-conid'>[]</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>TypeInference</span> <span class='hs-conid'>[]</span> <span class='hs-varid'>a</span></pre>

</div>

<div class="hint18 file9">
src/Strappy/Enumerate.hs:96:34: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-varid'>show</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>last</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>num</span> <span class='hs-varop'>&amp;&amp;</span> <span class='hs-varid'>num</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>n</span></pre>
Why not<br/>
<pre><span class='hs-definition'>trace</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>last</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>num</span> <span class='hs-varop'>&amp;&amp;</span> <span class='hs-varid'>num</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>n</span></pre>

</div>

<div class="hint3 file8">
src/Strappy/EnumWoMemo.hs:9:8: Error: Redundant do<br/>
Found<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>putStrLn</span> <span class='hs-varop'>$</span> <span class='hs-str'>"Finished. Total length "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-layout'>(</span><span class='hs-varid'>length</span> <span class='hs-varid'>out</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>putStrLn</span> <span class='hs-varop'>$</span> <span class='hs-str'>"Finished. Total length "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-layout'>(</span><span class='hs-varid'>length</span> <span class='hs-varid'>out</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint6 file10">
src/Strappy/Experiment.hs:2:1: Error: Unused LANGUAGE pragma<br/>
Found<br/>
<pre><span class='hs-comment'>{-# LANGUAGE ParallelListComp #-}</span></pre>
Why not remove it.<br/>
<pre></pre>

</div>

<div class="hint6 file11">
src/Strappy/Expr.hs:2:1: Error: Unused LANGUAGE pragma<br/>
Found<br/>
<pre><span class='hs-comment'>{-# LANGUAGE BangPatterns #-}</span></pre>
Why not remove it.<br/>
<pre></pre>

</div>

<div class="hint20 file11">
src/Strappy/Expr.hs:34:26: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>e</span> <span class='hs-varop'>==</span> <span class='hs-varid'>f</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>e</span> <span class='hs-varop'>==</span> <span class='hs-varid'>f</span></pre>

</div>

<div class="hint18 file11">
src/Strappy/Expr.hs:81:23: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-str'>"Reduction limit reached."</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-conid'>Nothing</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-str'>"Reduction limit reached."</span><span class='hs-layout'>)</span> <span class='hs-conid'>Nothing</span></pre>

</div>

<div class="hint18 file11">
src/Strappy/Expr.hs:81:24: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>trace</span> <span class='hs-varop'>$</span> <span class='hs-str'>"Reduction limit reached."</span></pre>
Why not<br/>
<pre><span class='hs-definition'>trace</span> <span class='hs-str'>"Reduction limit reached."</span></pre>

</div>

<div class="hint20 file12">
src/Strappy/Grammar.hs:59:38: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-definition'>ex</span> <span class='hs-conop'>:</span> <span class='hs-layout'>(</span><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>elems</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>ex</span> <span class='hs-conop'>:</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>elems</span> <span class='hs-varid'>lib</span></pre>

</div>

<div class="hint0 file12">
src/Strappy/Grammar.hs:66:1: Error: Eta reduce<br/>
Found<br/>
<pre><span class='hs-definition'>countExpansions</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>count</span> <span class='hs-num'>0</span> <span class='hs-varid'>c</span></pre>
Why not<br/>
<pre><span class='hs-definition'>countExpansions</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>count</span> <span class='hs-num'>0</span></pre>

</div>

<div class="hint18 file12">
src/Strappy/Grammar.hs:86:31: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>log</span> <span class='hs-varop'>$</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varid'>exp</span> <span class='hs-varid'>lp1</span><span class='hs-layout'>)</span> <span class='hs-varop'>*</span> <span class='hs-layout'>(</span><span class='hs-varid'>n</span><span class='hs-layout'>)</span> <span class='hs-varop'>+</span> <span class='hs-layout'>(</span><span class='hs-varid'>exp</span> <span class='hs-varid'>lp2</span><span class='hs-layout'>)</span> <span class='hs-varop'>*</span> <span class='hs-layout'>(</span><span class='hs-varid'>m</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>log</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varid'>exp</span> <span class='hs-varid'>lp1</span><span class='hs-layout'>)</span> <span class='hs-varop'>*</span> <span class='hs-layout'>(</span><span class='hs-varid'>n</span><span class='hs-layout'>)</span> <span class='hs-varop'>+</span> <span class='hs-layout'>(</span><span class='hs-varid'>exp</span> <span class='hs-varid'>lp2</span><span class='hs-layout'>)</span> <span class='hs-varop'>*</span> <span class='hs-layout'>(</span><span class='hs-varid'>m</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file12">
src/Strappy/Grammar.hs:86:38: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>exp</span> <span class='hs-varid'>lp1</span><span class='hs-layout'>)</span> <span class='hs-varop'>*</span> <span class='hs-layout'>(</span><span class='hs-varid'>n</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>exp</span> <span class='hs-varid'>lp1</span> <span class='hs-varop'>*</span> <span class='hs-layout'>(</span><span class='hs-varid'>n</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint2 file12">
src/Strappy/Grammar.hs:86:50: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>n</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>n</span></pre>

</div>

<div class="hint20 file12">
src/Strappy/Grammar.hs:87:34: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>exp</span> <span class='hs-varid'>lp2</span><span class='hs-layout'>)</span> <span class='hs-varop'>*</span> <span class='hs-layout'>(</span><span class='hs-varid'>m</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>exp</span> <span class='hs-varid'>lp2</span> <span class='hs-varop'>*</span> <span class='hs-layout'>(</span><span class='hs-varid'>m</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint2 file12">
src/Strappy/Grammar.hs:87:46: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>m</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>m</span></pre>

</div>

<div class="hint21 file12">
src/Strappy/Grammar.hs:93:1: Warning: Redundant guard<br/>
Found<br/>
<pre><span class='hs-definition'>bernLogProb</span> <span class='hs-varid'>hits</span> <span class='hs-varid'>obs</span>
  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span>
    <span class='hs-varid'>error</span> <span class='hs-varop'>$</span>
      <span class='hs-str'>"bernLogProb: # obs "</span> <span class='hs-varop'>++</span>
        <span class='hs-varid'>show</span> <span class='hs-varid'>obs</span> <span class='hs-varop'>++</span> <span class='hs-str'>" must be greater than # of hits "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>hits</span></pre>
Why not<br/>
<pre><span class='hs-definition'>bernLogProb</span> <span class='hs-varid'>hits</span> <span class='hs-varid'>obs</span>
  <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-varop'>$</span>
      <span class='hs-str'>"bernLogProb: # obs "</span> <span class='hs-varop'>++</span>
        <span class='hs-varid'>show</span> <span class='hs-varid'>obs</span> <span class='hs-varop'>++</span> <span class='hs-str'>" must be greater than # of hits "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>hits</span></pre>

</div>

<div class="hint18 file12">
src/Strappy/Grammar.hs:106:16: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>showCombMap</span> <span class='hs-varid'>ind</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>filter</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varop'>&gt;</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span> <span class='hs-varop'>.</span> <span class='hs-varid'>length</span><span class='hs-layout'>)</span> <span class='hs-varid'>ind</span></pre>
Why not<br/>
<pre><span class='hs-definition'>trace</span> <span class='hs-layout'>(</span><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>showCombMap</span> <span class='hs-varid'>ind</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>filter</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varop'>&gt;</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span> <span class='hs-varop'>.</span> <span class='hs-varid'>length</span><span class='hs-layout'>)</span> <span class='hs-varid'>ind</span></pre>

</div>

<div class="hint2 file12">
src/Strappy/Grammar.hs:109:55: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>combs</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>combs</span></pre>

</div>

<div class="hint25 file12">
src/Strappy/Grammar.hs:114:27: Warning: Use fromMaybe<br/>
Found<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-layout'>(</span><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>lookup</span> <span class='hs-varid'>c</span> <span class='hs-varid'>alts</span><span class='hs-layout'>)</span> <span class='hs-keyword'>of</span>
    <span class='hs-conid'>Nothing</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>error</span> <span class='hs-varop'>$</span>
                 <span class='hs-str'>"estimateGrammar: cannot find "</span> <span class='hs-varop'>++</span>
                   <span class='hs-varid'>show</span> <span class='hs-varid'>c</span> <span class='hs-varop'>++</span> <span class='hs-str'>" in alternative map "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>alts</span>
    <span class='hs-conid'>Just</span> <span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>k</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>fromMaybe</span>
   <span class='hs-layout'>(</span><span class='hs-varid'>error</span> <span class='hs-varop'>$</span>
      <span class='hs-str'>"estimateGrammar: cannot find "</span> <span class='hs-varop'>++</span>
        <span class='hs-varid'>show</span> <span class='hs-varid'>c</span> <span class='hs-varop'>++</span> <span class='hs-str'>" in alternative map "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>alts</span><span class='hs-layout'>)</span>
   <span class='hs-layout'>(</span><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>lookup</span> <span class='hs-varid'>c</span> <span class='hs-varid'>alts</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file12">
src/Strappy/Grammar.hs:114:27: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-layout'>(</span><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>lookup</span> <span class='hs-varid'>c</span> <span class='hs-varid'>alts</span><span class='hs-layout'>)</span> <span class='hs-keyword'>of</span>
    <span class='hs-conid'>Nothing</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>error</span> <span class='hs-varop'>$</span>
                 <span class='hs-str'>"estimateGrammar: cannot find "</span> <span class='hs-varop'>++</span>
                   <span class='hs-varid'>show</span> <span class='hs-varid'>c</span> <span class='hs-varop'>++</span> <span class='hs-str'>" in alternative map "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>alts</span>
    <span class='hs-conid'>Just</span> <span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>k</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>lookup</span> <span class='hs-varid'>c</span> <span class='hs-varid'>alts</span> <span class='hs-keyword'>of</span>
    <span class='hs-conid'>Nothing</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>error</span> <span class='hs-varop'>$</span>
                 <span class='hs-str'>"estimateGrammar: cannot find "</span> <span class='hs-varop'>++</span>
                   <span class='hs-varid'>show</span> <span class='hs-varid'>c</span> <span class='hs-varop'>++</span> <span class='hs-str'>" in alternative map "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>alts</span>
    <span class='hs-conid'>Just</span> <span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>k</span></pre>

</div>

<div class="hint20 file12">
src/Strappy/Grammar.hs:134:28: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>library</span> <span class='hs-varid'>gr</span><span class='hs-layout'>)</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.!</span> <span class='hs-varid'>x</span></pre>
Why not<br/>
<pre><span class='hs-definition'>library</span> <span class='hs-varid'>gr</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.!</span> <span class='hs-varid'>x</span></pre>

</div>

<div class="hint20 file12">
src/Strappy/Grammar.hs:136:25: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>library</span> <span class='hs-varid'>gr</span><span class='hs-layout'>)</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.!</span> <span class='hs-varid'>c</span></pre>
Why not<br/>
<pre><span class='hs-definition'>library</span> <span class='hs-varid'>gr</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.!</span> <span class='hs-varid'>c</span></pre>

</div>

<div class="hint2 file12">
src/Strappy/Grammar.hs:137:46: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-num'>0.5</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-num'>0.5</span></pre>

</div>

<div class="hint20 file13">
src/Strappy/GraphSearch.hs:17:32: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>edge</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-varid'>y</span> <span class='hs-conop'>:</span> <span class='hs-varid'>ex</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-varid'>edge</span> <span class='hs-varop'>+</span> <span class='hs-varid'>cost</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>edge</span><span class='hs-layout'>,</span> <span class='hs-varid'>y</span> <span class='hs-conop'>:</span> <span class='hs-varid'>ex</span><span class='hs-layout'>,</span> <span class='hs-varid'>edge</span> <span class='hs-varop'>+</span> <span class='hs-varid'>cost</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint26 file13">
src/Strappy/GraphSearch.hs:19:15: Warning: Use guards<br/>
Found<br/>
<pre><span class='hs-definition'>minBy</span> <span class='hs-varid'>x</span><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-varid'>edge</span><span class='hs-layout'>,</span> <span class='hs-varid'>ex</span><span class='hs-layout'>,</span> <span class='hs-varid'>cost</span><span class='hs-layout'>)</span> <span class='hs-varid'>y</span><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-varid'>edge'</span><span class='hs-layout'>,</span> <span class='hs-varid'>ex'</span><span class='hs-layout'>,</span> <span class='hs-varid'>cost'</span><span class='hs-layout'>)</span>
  <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>if</span> <span class='hs-varid'>cost</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>edge'</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>x</span> <span class='hs-keyword'>else</span> <span class='hs-keyword'>if</span> <span class='hs-varid'>cost</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>cost'</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>x</span> <span class='hs-keyword'>else</span> <span class='hs-varid'>y</span></pre>
Why not<br/>
<pre><span class='hs-definition'>minBy</span> <span class='hs-varid'>x</span><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-varid'>edge</span><span class='hs-layout'>,</span> <span class='hs-varid'>ex</span><span class='hs-layout'>,</span> <span class='hs-varid'>cost</span><span class='hs-layout'>)</span> <span class='hs-varid'>y</span><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-varid'>edge'</span><span class='hs-layout'>,</span> <span class='hs-varid'>ex'</span><span class='hs-layout'>,</span> <span class='hs-varid'>cost'</span><span class='hs-layout'>)</span>
  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>cost</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>edge'</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>x</span>
  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>cost</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>cost'</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>x</span>
  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>y</span></pre>

</div>

<div class="hint4 file13">
src/Strappy/GraphSearch.hs:20:21: Error: Redundant if<br/>
Found<br/>
<pre><span class='hs-keyword'>if</span> <span class='hs-varid'>cost</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>edge'</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>x</span> <span class='hs-keyword'>else</span> <span class='hs-keyword'>if</span> <span class='hs-varid'>cost</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>cost'</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>x</span> <span class='hs-keyword'>else</span> <span class='hs-varid'>y</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>if</span> <span class='hs-layout'>(</span><span class='hs-varid'>cost</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>edge'</span><span class='hs-layout'>)</span> <span class='hs-varop'>||</span> <span class='hs-layout'>(</span><span class='hs-varid'>cost</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>cost'</span><span class='hs-layout'>)</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>x</span> <span class='hs-keyword'>else</span> <span class='hs-varid'>y</span></pre>

</div>

<div class="hint2 file13">
src/Strappy/GraphSearch.hs:47:50: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>m</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>m</span></pre>

</div>

<div class="hint2 file13">
src/Strappy/GraphSearch.hs:50:25: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>n</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>n</span></pre>

</div>

<div class="hint4 file13">
src/Strappy/GraphSearch.hs:71:16: Error: Redundant if<br/>
Found<br/>
<pre><span class='hs-keyword'>if</span> <span class='hs-varid'>exCost</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>edge</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>x</span> <span class='hs-keyword'>else</span> <span class='hs-keyword'>if</span> <span class='hs-varid'>exCost</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>exCost'</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>x</span> <span class='hs-keyword'>else</span> <span class='hs-varid'>x'</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-keyword'>if</span> <span class='hs-layout'>(</span><span class='hs-varid'>exCost</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>edge</span><span class='hs-layout'>)</span> <span class='hs-varop'>||</span> <span class='hs-layout'>(</span><span class='hs-varid'>exCost</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>exCost'</span><span class='hs-layout'>)</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>x</span> <span class='hs-keyword'>else</span> <span class='hs-varid'>x'</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file13">
src/Strappy/GraphSearch.hs:81:18: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>x</span> <span class='hs-conop'>:</span> <span class='hs-varid'>cache</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>x</span> <span class='hs-conop'>:</span> <span class='hs-varid'>cache</span></pre>

</div>

<div class="hint23 file14">
src/Strappy/ParseCL.hs:64:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>p_expr</span> <span class='hs-varid'>lib</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>pExpr</span> <span class='hs-varid'>lib</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint20 file14">
src/Strappy/ParseCL.hs:74:17: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>node</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-varop'>&lt;|&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>p_expr</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>node</span> <span class='hs-varid'>lib</span> <span class='hs-varop'>&lt;|&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>p_expr</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file14">
src/Strappy/ParseCL.hs:74:17: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>node</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-varop'>&lt;|&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>p_expr</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>node</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-varop'>&lt;|&gt;</span> <span class='hs-varid'>p_expr</span> <span class='hs-varid'>lib</span></pre>

</div>

<div class="hint10 file15">
src/Strappy/PostProcess.hs:15:1: Error: Use fewer imports<br/>
Found<br/>
<pre><span class='hs-keyword'>import</span> <span class='hs-conid'>Text</span><span class='hs-varop'>.</span><span class='hs-conid'>ParserCombinators</span><span class='hs-varop'>.</span><span class='hs-conid'>Parsec</span>
<span class='hs-keyword'>import</span> <span class='hs-conid'>Text</span><span class='hs-varop'>.</span><span class='hs-conid'>ParserCombinators</span><span class='hs-varop'>.</span><span class='hs-conid'>Parsec</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varop'>&lt;|&gt;</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-varop'>&lt;?&gt;</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
</pre>
Why not<br/>
<pre><span class='hs-keyword'>import</span> <span class='hs-conid'>Text</span><span class='hs-varop'>.</span><span class='hs-conid'>ParserCombinators</span><span class='hs-varop'>.</span><span class='hs-conid'>Parsec</span>
</pre>

</div>

<div class="hint20 file15">
src/Strappy/PostProcess.hs:43:27: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>lookup</span> <span class='hs-varid'>c</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-layout'>(</span><span class='hs-varid'>getDataForComb</span> <span class='hs-varid'>c</span> <span class='hs-varid'>ss</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>lookup</span> <span class='hs-varid'>c</span> <span class='hs-varid'>lib</span> <span class='hs-conop'>:</span> <span class='hs-layout'>(</span><span class='hs-varid'>getDataForComb</span> <span class='hs-varid'>c</span> <span class='hs-varid'>ss</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file15">
src/Strappy/PostProcess.hs:43:27: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>lookup</span> <span class='hs-varid'>c</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-layout'>(</span><span class='hs-varid'>getDataForComb</span> <span class='hs-varid'>c</span> <span class='hs-varid'>ss</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>lookup</span> <span class='hs-varid'>c</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>getDataForComb</span> <span class='hs-varid'>c</span> <span class='hs-varid'>ss</span></pre>

</div>

<div class="hint20 file15">
src/Strappy/PostProcess.hs:52:35: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-str'>", "</span> <span class='hs-varop'>++</span> <span class='hs-layout'>(</span><span class='hs-varid'>toCSV</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-str'>", "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>toCSV</span> <span class='hs-varid'>v</span></pre>

</div>

<div class="hint0 file15">
src/Strappy/PostProcess.hs:60:1: Error: Eta reduce<br/>
Found<br/>
<pre><span class='hs-definition'>getNumHitByIter</span> <span class='hs-varid'>ss</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>length</span> <span class='hs-varop'>.</span> <span class='hs-varid'>searchExplanation</span><span class='hs-layout'>)</span> <span class='hs-varid'>ss</span></pre>
Why not<br/>
<pre><span class='hs-definition'>getNumHitByIter</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>length</span> <span class='hs-varop'>.</span> <span class='hs-varid'>searchExplanation</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint0 file15">
src/Strappy/PostProcess.hs:63:1: Error: Eta reduce<br/>
Found<br/>
<pre><span class='hs-definition'>getExplanationsByIter</span> <span class='hs-varid'>ss</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>searchExplanation</span><span class='hs-layout'>)</span> <span class='hs-varid'>ss</span></pre>
Why not<br/>
<pre><span class='hs-definition'>getExplanationsByIter</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>searchExplanation</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint2 file15">
src/Strappy/PostProcess.hs:63:32: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>searchExplanation</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>searchExplanation</span></pre>

</div>

<div class="hint20 file15">
src/Strappy/PostProcess.hs:65:24: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>foldl1</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-varid'>a</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-varop'>++</span> <span class='hs-str'>", "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>map</span> <span class='hs-varid'>show</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>foldl1</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-varid'>a</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-varop'>++</span> <span class='hs-str'>", "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>map</span> <span class='hs-varid'>show</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file15">
src/Strappy/PostProcess.hs:70:30: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>foldl1</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-varid'>a</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-varop'>++</span> <span class='hs-str'>"\n \n"</span> <span class='hs-varop'>++</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>map</span> <span class='hs-varid'>show</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>foldl1</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-varid'>a</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-varop'>++</span> <span class='hs-str'>"\n \n"</span> <span class='hs-varop'>++</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>map</span> <span class='hs-varid'>show</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint0 file15">
src/Strappy/PostProcess.hs:74:1: Error: Eta reduce<br/>
Found<br/>
<pre><span class='hs-definition'>spaceToUnderscore</span> <span class='hs-varid'>str</span>
  <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-varid'>x</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>if</span> <span class='hs-varid'>x</span> <span class='hs-varop'>==</span> <span class='hs-chr'>' '</span> <span class='hs-keyword'>then</span> <span class='hs-chr'>'_'</span> <span class='hs-keyword'>else</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-varid'>str</span></pre>
Why not<br/>
<pre><span class='hs-definition'>spaceToUnderscore</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-varid'>x</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>if</span> <span class='hs-varid'>x</span> <span class='hs-varop'>==</span> <span class='hs-chr'>' '</span> <span class='hs-keyword'>then</span> <span class='hs-chr'>'_'</span> <span class='hs-keyword'>else</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint0 file15">
src/Strappy/PostProcess.hs:75:1: Error: Eta reduce<br/>
Found<br/>
<pre><span class='hs-definition'>colonToDash</span> <span class='hs-varid'>str</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-varid'>x</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>if</span> <span class='hs-varid'>x</span> <span class='hs-varop'>==</span> <span class='hs-chr'>':'</span> <span class='hs-keyword'>then</span> <span class='hs-chr'>'-'</span> <span class='hs-keyword'>else</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-varid'>str</span></pre>
Why not<br/>
<pre><span class='hs-definition'>colonToDash</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-varid'>x</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>if</span> <span class='hs-varid'>x</span> <span class='hs-varop'>==</span> <span class='hs-chr'>':'</span> <span class='hs-keyword'>then</span> <span class='hs-chr'>'-'</span> <span class='hs-keyword'>else</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint18 file15">
src/Strappy/PostProcess.hs:81:35: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>reformat</span> <span class='hs-varop'>$</span> <span class='hs-varid'>expName</span> <span class='hs-varid'>exp</span><span class='hs-layout'>)</span> <span class='hs-varop'>++</span> <span class='hs-str'>"_"</span> <span class='hs-varop'>++</span> <span class='hs-varid'>timeStr'</span></pre>
Why not<br/>
<pre><span class='hs-definition'>reformat</span> <span class='hs-layout'>(</span><span class='hs-varid'>expName</span> <span class='hs-varid'>exp</span><span class='hs-layout'>)</span> <span class='hs-varop'>++</span> <span class='hs-str'>"_"</span> <span class='hs-varop'>++</span> <span class='hs-varid'>timeStr'</span></pre>

</div>

<div class="hint18 file15">
src/Strappy/PostProcess.hs:106:42: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>getLibData</span> <span class='hs-varop'>$</span> <span class='hs-varid'>searchData</span></pre>
Why not<br/>
<pre><span class='hs-definition'>getLibData</span> <span class='hs-varid'>searchData</span></pre>

</div>

<div class="hint18 file15">
src/Strappy/PostProcess.hs:107:44: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>getNumHitByIter</span> <span class='hs-varop'>$</span> <span class='hs-varid'>searchData</span></pre>
Why not<br/>
<pre><span class='hs-definition'>getNumHitByIter</span> <span class='hs-varid'>searchData</span></pre>

</div>

<div class="hint18 file15">
src/Strappy/PostProcess.hs:108:48: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>getExplanationsByIter</span> <span class='hs-varop'>$</span> <span class='hs-varid'>searchData</span></pre>
Why not<br/>
<pre><span class='hs-definition'>getExplanationsByIter</span> <span class='hs-varid'>searchData</span></pre>

</div>

<div class="hint28 file15">
src/Strappy/PostProcess.hs:120:15: Warning: Use map once<br/>
Found<br/>
<pre><span class='hs-definition'>map</span> <span class='hs-varid'>head</span> <span class='hs-varop'>$</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>splitOn</span> <span class='hs-str'>","</span><span class='hs-layout'>)</span> <span class='hs-varid'>rows</span></pre>
Why not<br/>
<pre><span class='hs-definition'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>head</span> <span class='hs-varop'>.</span> <span class='hs-varid'>splitOn</span> <span class='hs-str'>","</span><span class='hs-layout'>)</span> <span class='hs-varid'>rows</span></pre>

</div>

<div class="hint20 file15">
src/Strappy/PostProcess.hs:123:35: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>str</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>readFile</span> <span class='hs-varid'>filename</span>
   <span class='hs-keyword'>let</span> <span class='hs-varid'>rows</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>splitOn</span> <span class='hs-str'>"\n"</span> <span class='hs-varid'>str</span>
       <span class='hs-varid'>splitrows'</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>splitOn</span> <span class='hs-str'>","</span><span class='hs-layout'>)</span> <span class='hs-varid'>rows</span>
       <span class='hs-varid'>combstrs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-varid'>head</span> <span class='hs-varop'>$</span> <span class='hs-varid'>splitrows'</span>
       <span class='hs-varid'>valstrs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-varid'>last</span> <span class='hs-varop'>$</span> <span class='hs-varid'>splitrows'</span>
       <span class='hs-varid'>pairstrs</span>
         <span class='hs-keyglyph'>=</span> <span class='hs-varid'>filter</span> <span class='hs-layout'>(</span><span class='hs-varid'>not</span> <span class='hs-varop'>.</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-varid'>x</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>isInfixOf</span> <span class='hs-str'>"nan"</span> <span class='hs-varid'>x</span> <span class='hs-varop'>||</span> <span class='hs-varid'>null</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-varop'>.</span> <span class='hs-varid'>snd</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span>
             <span class='hs-varid'>zip</span> <span class='hs-varid'>combstrs</span> <span class='hs-varid'>valstrs</span>
       <span class='hs-varid'>combs</span>
         <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-varid'>show</span> <span class='hs-varid'>pairstrs</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span>
             <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromRight</span> <span class='hs-varop'>.</span> <span class='hs-varid'>parseExpr</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>map</span> <span class='hs-varid'>fst</span> <span class='hs-varid'>pairstrs</span>
       <span class='hs-varid'>vals</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>read</span> <span class='hs-varop'>.</span> <span class='hs-varid'>snd</span><span class='hs-layout'>)</span> <span class='hs-varid'>pairstrs</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Double</span><span class='hs-keyglyph'>]</span>
       <span class='hs-varid'>lib'</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-varop'>$</span> <span class='hs-varid'>zip</span> <span class='hs-varid'>combs</span> <span class='hs-varid'>vals</span><span class='hs-layout'>)</span>
       <span class='hs-varid'>grammar</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Grammar</span> <span class='hs-varid'>lib'</span> <span class='hs-num'>0</span>
   <span class='hs-varid'>return</span> <span class='hs-varid'>grammar</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>str</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>readFile</span> <span class='hs-varid'>filename</span>
   <span class='hs-keyword'>let</span> <span class='hs-varid'>rows</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>splitOn</span> <span class='hs-str'>"\n"</span> <span class='hs-varid'>str</span>
       <span class='hs-varid'>splitrows'</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>splitOn</span> <span class='hs-str'>","</span><span class='hs-layout'>)</span> <span class='hs-varid'>rows</span>
       <span class='hs-varid'>combstrs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-varid'>head</span> <span class='hs-varop'>$</span> <span class='hs-varid'>splitrows'</span>
       <span class='hs-varid'>valstrs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-varid'>last</span> <span class='hs-varop'>$</span> <span class='hs-varid'>splitrows'</span>
       <span class='hs-varid'>pairstrs</span>
         <span class='hs-keyglyph'>=</span> <span class='hs-varid'>filter</span> <span class='hs-layout'>(</span><span class='hs-varid'>not</span> <span class='hs-varop'>.</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-varid'>x</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>isInfixOf</span> <span class='hs-str'>"nan"</span> <span class='hs-varid'>x</span> <span class='hs-varop'>||</span> <span class='hs-varid'>null</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-varop'>.</span> <span class='hs-varid'>snd</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span>
             <span class='hs-varid'>zip</span> <span class='hs-varid'>combstrs</span> <span class='hs-varid'>valstrs</span>
       <span class='hs-varid'>combs</span>
         <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-varid'>show</span> <span class='hs-varid'>pairstrs</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span>
             <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromRight</span> <span class='hs-varop'>.</span> <span class='hs-varid'>parseExpr</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>map</span> <span class='hs-varid'>fst</span> <span class='hs-varid'>pairstrs</span>
       <span class='hs-varid'>vals</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>read</span> <span class='hs-varop'>.</span> <span class='hs-varid'>snd</span><span class='hs-layout'>)</span> <span class='hs-varid'>pairstrs</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Double</span><span class='hs-keyglyph'>]</span>
       <span class='hs-varid'>lib'</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-varop'>$</span> <span class='hs-varid'>zip</span> <span class='hs-varid'>combs</span> <span class='hs-varid'>vals</span>
       <span class='hs-varid'>grammar</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Grammar</span> <span class='hs-varid'>lib'</span> <span class='hs-num'>0</span>
   <span class='hs-varid'>return</span> <span class='hs-varid'>grammar</span></pre>

</div>

<div class="hint18 file15">
src/Strappy/PostProcess.hs:127:18: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>map</span> <span class='hs-varid'>head</span> <span class='hs-varop'>$</span> <span class='hs-varid'>splitrows'</span></pre>
Why not<br/>
<pre><span class='hs-definition'>map</span> <span class='hs-varid'>head</span> <span class='hs-varid'>splitrows'</span></pre>

</div>

<div class="hint18 file15">
src/Strappy/PostProcess.hs:128:17: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>map</span> <span class='hs-varid'>last</span> <span class='hs-varop'>$</span> <span class='hs-varid'>splitrows'</span></pre>
Why not<br/>
<pre><span class='hs-definition'>map</span> <span class='hs-varid'>last</span> <span class='hs-varid'>splitrows'</span></pre>

</div>

<div class="hint18 file15">
src/Strappy/PostProcess.hs:130:15: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-varid'>show</span> <span class='hs-varid'>pairstrs</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span>
  <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromRight</span> <span class='hs-varop'>.</span> <span class='hs-varid'>parseExpr</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>map</span> <span class='hs-varid'>fst</span> <span class='hs-varid'>pairstrs</span></pre>
Why not<br/>
<pre><span class='hs-definition'>trace</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varid'>pairstrs</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span>
  <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromRight</span> <span class='hs-varop'>.</span> <span class='hs-varid'>parseExpr</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>map</span> <span class='hs-varid'>fst</span> <span class='hs-varid'>pairstrs</span></pre>

</div>

<div class="hint28 file15">
src/Strappy/PostProcess.hs:130:41: Warning: Use map once<br/>
Found<br/>
<pre><span class='hs-definition'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromRight</span> <span class='hs-varop'>.</span> <span class='hs-varid'>parseExpr</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>map</span> <span class='hs-varid'>fst</span> <span class='hs-varid'>pairstrs</span></pre>
Why not<br/>
<pre><span class='hs-definition'>map</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varid'>fromRight</span> <span class='hs-varop'>.</span> <span class='hs-varid'>parseExpr</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-varop'>.</span> <span class='hs-varid'>fst</span><span class='hs-layout'>)</span> <span class='hs-varid'>pairstrs</span></pre>

</div>

<div class="hint28 file15">
src/Strappy/PostProcess.hs:139:23: Warning: Use map once<br/>
Found<br/>
<pre><span class='hs-definition'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromRight</span> <span class='hs-varop'>.</span> <span class='hs-layout'>(</span><span class='hs-varid'>parseExpr</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span>
  <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>map</span> <span class='hs-varid'>snd</span><span class='hs-layout'>)</span> <span class='hs-varid'>explStrTuples</span></pre>
Why not<br/>
<pre><span class='hs-definition'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromRight</span> <span class='hs-varop'>.</span> <span class='hs-layout'>(</span><span class='hs-varid'>parseExpr</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-varop'>.</span> <span class='hs-varid'>map</span> <span class='hs-varid'>snd</span><span class='hs-layout'>)</span> <span class='hs-varid'>explStrTuples</span></pre>

</div>

<div class="hint20 file15">
src/Strappy/PostProcess.hs:139:33: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-definition'>fromRight</span> <span class='hs-varop'>.</span> <span class='hs-layout'>(</span><span class='hs-varid'>parseExpr</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>fromRight</span> <span class='hs-varop'>.</span> <span class='hs-varid'>parseExpr</span> <span class='hs-varid'>lib</span></pre>

</div>

<div class="hint20 file15">
src/Strappy/PostProcess.hs:169:18: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>char</span> <span class='hs-chr'>','</span> <span class='hs-varop'>&gt;&gt;</span> <span class='hs-varid'>cells</span><span class='hs-layout'>)</span> <span class='hs-varop'>&lt;|&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>return</span> <span class='hs-conid'>[]</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>char</span> <span class='hs-chr'>','</span> <span class='hs-varop'>&gt;&gt;</span> <span class='hs-varid'>cells</span><span class='hs-layout'>)</span> <span class='hs-varop'>&lt;|&gt;</span> <span class='hs-varid'>return</span> <span class='hs-conid'>[]</span></pre>

</div>

<div class="hint23 file16">
src/Strappy/Routers.hs:27:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>one_routers</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>oneRouters</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint18 file16">
src/Strappy/Routers.hs:27:15: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-varop'>$</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>cName</span> <span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>cS</span><span class='hs-layout'>,</span> <span class='hs-varid'>cC</span><span class='hs-layout'>,</span> <span class='hs-varid'>cB</span><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span></pre>
Why not<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>cName</span> <span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>cS</span><span class='hs-layout'>,</span> <span class='hs-varid'>cC</span><span class='hs-layout'>,</span> <span class='hs-varid'>cB</span><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span></pre>

</div>

<div class="hint23 file16">
src/Strappy/Routers.hs:29:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>two_routers</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>twoRouters</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint18 file16">
src/Strappy/Routers.hs:29:15: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-varop'>$</span>
  <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>cName</span> <span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>cSS</span><span class='hs-layout'>,</span> <span class='hs-varid'>cSB</span><span class='hs-layout'>,</span> <span class='hs-varid'>cSC</span><span class='hs-layout'>,</span> <span class='hs-varid'>cBS</span><span class='hs-layout'>,</span> <span class='hs-varid'>cBB</span><span class='hs-layout'>,</span> <span class='hs-varid'>cBC</span><span class='hs-layout'>,</span> <span class='hs-varid'>cCS</span><span class='hs-layout'>,</span> <span class='hs-varid'>cCB</span><span class='hs-layout'>,</span> <span class='hs-varid'>cCC</span><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span></pre>
Why not<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span>
  <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>cName</span> <span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>cSS</span><span class='hs-layout'>,</span> <span class='hs-varid'>cSB</span><span class='hs-layout'>,</span> <span class='hs-varid'>cSC</span><span class='hs-layout'>,</span> <span class='hs-varid'>cBS</span><span class='hs-layout'>,</span> <span class='hs-varid'>cBB</span><span class='hs-layout'>,</span> <span class='hs-varid'>cBC</span><span class='hs-layout'>,</span> <span class='hs-varid'>cCS</span><span class='hs-layout'>,</span> <span class='hs-varid'>cCB</span><span class='hs-layout'>,</span> <span class='hs-varid'>cCC</span><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span></pre>

</div>

<div class="hint23 file16">
src/Strappy/Routers.hs:32:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>three_routers</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>threeRouters</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint18 file16">
src/Strappy/Routers.hs:32:17: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-varop'>$</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>cName</span> <span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>ternaryRouters</span><span class='hs-keyglyph'>]</span></pre>
Why not<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>cName</span> <span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>ternaryRouters</span><span class='hs-keyglyph'>]</span></pre>

</div>

<div class="hint2 file16">
src/Strappy/Routers.hs:117:39: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>g</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>g</span></pre>

</div>

<div class="hint2 file16">
src/Strappy/Routers.hs:343:17: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>f</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>f</span></pre>

</div>

<div class="hint20 file17">
src/Strappy/Run.hs:35:30: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>expNumBound</span> <span class='hs-varid'>ex</span><span class='hs-layout'>)</span> <span class='hs-varop'>*</span> <span class='hs-layout'>(</span><span class='hs-varid'>expReps</span> <span class='hs-varid'>ex</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>expNumBound</span> <span class='hs-varid'>ex</span> <span class='hs-varop'>*</span> <span class='hs-layout'>(</span><span class='hs-varid'>expReps</span> <span class='hs-varid'>ex</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file17">
src/Strappy/Run.hs:35:30: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>expNumBound</span> <span class='hs-varid'>ex</span><span class='hs-layout'>)</span> <span class='hs-varop'>*</span> <span class='hs-layout'>(</span><span class='hs-varid'>expReps</span> <span class='hs-varid'>ex</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>expNumBound</span> <span class='hs-varid'>ex</span><span class='hs-layout'>)</span> <span class='hs-varop'>*</span> <span class='hs-varid'>expReps</span> <span class='hs-varid'>ex</span></pre>

</div>

<div class="hint20 file17">
src/Strappy/Run.hs:36:33: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>expName</span> <span class='hs-varid'>ex</span><span class='hs-layout'>)</span> <span class='hs-varop'>++</span> <span class='hs-str'>"_Brute"</span></pre>
Why not<br/>
<pre><span class='hs-definition'>expName</span> <span class='hs-varid'>ex</span> <span class='hs-varop'>++</span> <span class='hs-str'>"_Brute"</span></pre>

</div>

<div class="hint27 file18">
src/Strappy/Sample.hs:30:30: Warning: Use if<br/>
Found<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-varid'>shouldExpand</span> <span class='hs-keyword'>of</span>
    <span class='hs-conid'>True</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span> <span class='hs-varid'>t</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>newTVar</span> <span class='hs-conid'>Star</span>
               <span class='hs-varid'>c_left</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>sampleFromGrammar</span> <span class='hs-varid'>gr</span> <span class='hs-layout'>(</span><span class='hs-varid'>t</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>tp</span><span class='hs-layout'>)</span>
               <span class='hs-varid'>c_right</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>sampleFromGrammar</span> <span class='hs-varid'>gr</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromType</span> <span class='hs-layout'>(</span><span class='hs-varid'>cType</span> <span class='hs-varid'>c_left</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
               <span class='hs-varid'>return</span> <span class='hs-varop'>$</span> <span class='hs-varid'>c_left</span> <span class='hs-varop'>`app'`</span> <span class='hs-varid'>c_right</span>
    <span class='hs-conid'>False</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span> <span class='hs-keyword'>let</span> <span class='hs-varid'>lib</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>library</span> <span class='hs-varid'>gr</span>
                <span class='hs-keyword'>let</span> <span class='hs-varid'>cs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>runStateT</span> <span class='hs-layout'>(</span><span class='hs-varid'>filterCombinatorsByType</span> <span class='hs-layout'>(</span><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>keys</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-varid'>tp</span><span class='hs-layout'>)</span> <span class='hs-num'>0</span>
                <span class='hs-keyword'>let</span> <span class='hs-varid'>dist</span> <span class='hs-keyglyph'>=</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>lib</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.!</span> <span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>cs</span><span class='hs-keyglyph'>]</span>
                    <span class='hs-varid'>z</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>logsumexp</span> <span class='hs-varop'>.</span> <span class='hs-varid'>map</span> <span class='hs-varid'>fst</span> <span class='hs-varop'>$</span> <span class='hs-varid'>dist</span>
                    <span class='hs-varid'>dist'</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-layout'>,</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>exp</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span> <span class='hs-comment'>-</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>dist</span>
                <span class='hs-varid'>guard</span> <span class='hs-layout'>(</span><span class='hs-varid'>not</span> <span class='hs-varop'>.</span> <span class='hs-varid'>null</span> <span class='hs-varop'>$</span> <span class='hs-varid'>dist</span><span class='hs-layout'>)</span>
                <span class='hs-varid'>sampleMultinomial</span> <span class='hs-varid'>dist'</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>if</span> <span class='hs-varid'>shouldExpand</span> <span class='hs-keyword'>then</span>
  <span class='hs-layout'>(</span><span class='hs-keyword'>do</span> <span class='hs-varid'>t</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>newTVar</span> <span class='hs-conid'>Star</span>
      <span class='hs-varid'>c_left</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>sampleFromGrammar</span> <span class='hs-varid'>gr</span> <span class='hs-layout'>(</span><span class='hs-varid'>t</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>tp</span><span class='hs-layout'>)</span>
      <span class='hs-varid'>c_right</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>sampleFromGrammar</span> <span class='hs-varid'>gr</span> <span class='hs-layout'>(</span><span class='hs-varid'>fromType</span> <span class='hs-layout'>(</span><span class='hs-varid'>cType</span> <span class='hs-varid'>c_left</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
      <span class='hs-varid'>return</span> <span class='hs-varop'>$</span> <span class='hs-varid'>c_left</span> <span class='hs-varop'>`app'`</span> <span class='hs-varid'>c_right</span><span class='hs-layout'>)</span>
  <span class='hs-keyword'>else</span>
  <span class='hs-layout'>(</span><span class='hs-keyword'>do</span> <span class='hs-keyword'>let</span> <span class='hs-varid'>lib</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>library</span> <span class='hs-varid'>gr</span>
      <span class='hs-keyword'>let</span> <span class='hs-varid'>cs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>runStateT</span> <span class='hs-layout'>(</span><span class='hs-varid'>filterCombinatorsByType</span> <span class='hs-layout'>(</span><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>keys</span> <span class='hs-varid'>lib</span><span class='hs-layout'>)</span> <span class='hs-varid'>tp</span><span class='hs-layout'>)</span> <span class='hs-num'>0</span>
      <span class='hs-keyword'>let</span> <span class='hs-varid'>dist</span> <span class='hs-keyglyph'>=</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>lib</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.!</span> <span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>cs</span><span class='hs-keyglyph'>]</span>
          <span class='hs-varid'>z</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>logsumexp</span> <span class='hs-varop'>.</span> <span class='hs-varid'>map</span> <span class='hs-varid'>fst</span> <span class='hs-varop'>$</span> <span class='hs-varid'>dist</span>
          <span class='hs-varid'>dist'</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-layout'>,</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>exp</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span> <span class='hs-comment'>-</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>dist</span>
      <span class='hs-varid'>guard</span> <span class='hs-layout'>(</span><span class='hs-varid'>not</span> <span class='hs-varop'>.</span> <span class='hs-varid'>null</span> <span class='hs-varop'>$</span> <span class='hs-varid'>dist</span><span class='hs-layout'>)</span>
      <span class='hs-varid'>sampleMultinomial</span> <span class='hs-varid'>dist'</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint18 file18">
src/Strappy/Sample.hs:39:57: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>map</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-layout'>,</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>exp</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span> <span class='hs-comment'>-</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>dist</span></pre>
Why not<br/>
<pre><span class='hs-definition'>map</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>\</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-layout'>,</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>exp</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span> <span class='hs-comment'>-</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-varid'>dist</span></pre>

</div>

<div class="hint16 file19">
src/Strappy/Search.hs:80:46: Warning: Parse error<br/>
Error message<br/>
<pre>Parse error: Last statement in a do-block must be an expression</pre>
Code<br/>
<pre>           <span class='hs-varid'>return</span> <span class='hs-varop'>$</span> <span class='hs-keyword'>do</span> 
             <span class='hs-varid'>t</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>taskSet</span>
<span class='hs-varop'>&gt;</span>            <span class='hs-keyword'>let</span> <span class='hs-varid'>cs</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-varid'>show</span> <span class='hs-varid'>t</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span><span class='hs-keyword'>case</span> <span class='hs-varid'>t</span> <span class='hs-keyword'>of</span> 
                        <span class='hs-conid'>Task</span> <span class='hs-varid'>n</span> <span class='hs-varid'>f</span> <span class='hs-varid'>tp</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>db</span> <span class='hs-conid'>Map</span><span class='hs-varop'>.!</span> <span class='hs-varid'>tp</span>
                                          <span class='hs-keyword'>if</span> <span class='hs-varid'>f</span> <span class='hs-varid'>c</span> <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>eps</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>return</span> <span class='hs-varid'>c</span> <span class='hs-keyword'>else</span> <span class='hs-varid'>mzero</span>
</pre>
</div>

<div class="hint26 file20">
src/Strappy/SimpleCL.hs:27:1: Warning: Use guards<br/>
Found<br/>
<pre><span class='hs-definition'>reduce</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span>
  <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>if</span> <span class='hs-varid'>redex</span> <span class='hs-varid'>x</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>reduce</span> <span class='hs-layout'>(</span><span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyword'>else</span>
      <span class='hs-keyword'>if</span> <span class='hs-varid'>redex</span> <span class='hs-varid'>y</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>reduce</span> <span class='hs-layout'>(</span><span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyword'>else</span>
        <span class='hs-layout'>(</span><span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>reduce</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span>
  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>redex</span> <span class='hs-varid'>x</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>reduce</span> <span class='hs-layout'>(</span><span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span>
  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>redex</span> <span class='hs-varid'>y</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>reduce</span> <span class='hs-layout'>(</span><span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span>
  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span></pre>

</div>

<div class="hint4 file20">
src/Strappy/SimpleCL.hs:27:19: Error: Redundant if<br/>
Found<br/>
<pre><span class='hs-keyword'>if</span> <span class='hs-varid'>redex</span> <span class='hs-varid'>x</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>reduce</span> <span class='hs-layout'>(</span><span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyword'>else</span>
  <span class='hs-keyword'>if</span> <span class='hs-varid'>redex</span> <span class='hs-varid'>y</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>reduce</span> <span class='hs-layout'>(</span><span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyword'>else</span>
    <span class='hs-layout'>(</span><span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>if</span> <span class='hs-varid'>redex</span> <span class='hs-varid'>x</span> <span class='hs-varop'>||</span> <span class='hs-varid'>redex</span> <span class='hs-varid'>y</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>reduce</span> <span class='hs-layout'>(</span><span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyword'>else</span>
  <span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span></pre>

</div>

<div class="hint20 file20">
src/Strappy/SimpleCL.hs:28:24: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-keyword'>if</span> <span class='hs-varid'>redex</span> <span class='hs-varid'>y</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>reduce</span> <span class='hs-layout'>(</span><span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyword'>else</span>
  <span class='hs-layout'>(</span><span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>if</span> <span class='hs-varid'>redex</span> <span class='hs-varid'>y</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>reduce</span> <span class='hs-layout'>(</span><span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyword'>else</span>
  <span class='hs-varid'>reduce</span> <span class='hs-varid'>x</span> <span class='hs-conop'>:&gt;</span> <span class='hs-varid'>reduce</span> <span class='hs-varid'>y</span></pre>

</div>

<div class="hint6 file21">
src/Strappy/StdLib.hs:2:1: Error: Unused LANGUAGE pragma<br/>
Found<br/>
<pre><span class='hs-comment'>{-# LANGUAGE BangPatterns #-}</span></pre>
Why not remove it.<br/>
<pre></pre>

</div>

<div class="hint24 file21">
src/Strappy/StdLib.hs:35:37: Warning: Use const<br/>
Found<br/>
<pre><span class='hs-keyglyph'>\</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span></pre>
Why not<br/>
<pre><span class='hs-definition'>const</span> <span class='hs-varid'>a</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:99:23: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-keyword'>if</span> <span class='hs-varid'>i</span> <span class='hs-varop'>&lt;=</span> <span class='hs-num'>0</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>c</span> <span class='hs-keyword'>else</span>
  <span class='hs-layout'>(</span><span class='hs-conid'>App</span> <span class='hs-varid'>f</span> <span class='hs-layout'>(</span><span class='hs-conid'>App</span> <span class='hs-layout'>(</span><span class='hs-conid'>App</span> <span class='hs-layout'>(</span><span class='hs-conid'>App</span> <span class='hs-varid'>prim</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-varid'>f</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>N</span> <span class='hs-varop'>$</span> <span class='hs-varid'>i</span> <span class='hs-comment'>-</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>if</span> <span class='hs-varid'>i</span> <span class='hs-varop'>&lt;=</span> <span class='hs-num'>0</span> <span class='hs-keyword'>then</span> <span class='hs-varid'>c</span> <span class='hs-keyword'>else</span> <span class='hs-conid'>App</span> <span class='hs-varid'>f</span> <span class='hs-layout'>(</span><span class='hs-conid'>App</span> <span class='hs-layout'>(</span><span class='hs-conid'>App</span> <span class='hs-layout'>(</span><span class='hs-conid'>App</span> <span class='hs-varid'>prim</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-varid'>f</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>N</span> <span class='hs-varop'>$</span> <span class='hs-varid'>i</span> <span class='hs-comment'>-</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:139:16: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-definition'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tMaybe</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-conid'>TAp</span> <span class='hs-varid'>tMaybe</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:142:16: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tMaybe</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-conid'>TAp</span> <span class='hs-varid'>tMaybe</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:153:32: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-definition'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:153:32: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-definition'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:160:16: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>
Why not<br/>
<pre><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:167:16: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:167:16: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:173:32: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-varid'>xs</span> <span class='hs-keyword'>of</span>
    <span class='hs-conid'>Const</span> <span class='hs-str'>"[]"</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>B</span> <span class='hs-conid'>True</span><span class='hs-layout'>)</span>
    <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>B</span> <span class='hs-conid'>False</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-varid'>xs</span> <span class='hs-keyword'>of</span>
    <span class='hs-conid'>Const</span> <span class='hs-str'>"[]"</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>B</span> <span class='hs-conid'>True</span>
    <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>B</span> <span class='hs-conid'>False</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:173:32: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-varid'>xs</span> <span class='hs-keyword'>of</span>
    <span class='hs-conid'>Const</span> <span class='hs-str'>"[]"</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>B</span> <span class='hs-conid'>True</span><span class='hs-layout'>)</span>
    <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>B</span> <span class='hs-conid'>False</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-varid'>xs</span> <span class='hs-keyword'>of</span>
    <span class='hs-conid'>Const</span> <span class='hs-str'>"[]"</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>B</span> <span class='hs-conid'>True</span><span class='hs-layout'>)</span>
    <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>B</span> <span class='hs-conid'>False</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:176:16: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>tBool</span></pre>
Why not<br/>
<pre><span class='hs-conid'>TAp</span> <span class='hs-varid'>tList</span> <span class='hs-varid'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>tBool</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:186:19: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tPair</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>
Why not<br/>
<pre><span class='hs-conid'>TAp</span> <span class='hs-varid'>tPair</span> <span class='hs-varid'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:192:19: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tPair</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>
Why not<br/>
<pre><span class='hs-conid'>TAp</span> <span class='hs-varid'>tPair</span> <span class='hs-varid'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:201:20: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>
Why not<br/>
<pre><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:206:20: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>
Why not<br/>
<pre><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:211:20: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>
Why not<br/>
<pre><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:219:20: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>
Why not<br/>
<pre><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:224:20: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>
Why not<br/>
<pre><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:229:20: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>
Why not<br/>
<pre><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:234:20: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span><span class='hs-layout'>)</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>
Why not<br/>
<pre><span class='hs-conid'>TAp</span> <span class='hs-varid'>tTriple</span> <span class='hs-varid'>t0</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t0</span></pre>

</div>

<div class="hint18 file21">
src/Strappy/StdLib.hs:237:12: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-varop'>$</span>
  <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-str'>"I"</span><span class='hs-layout'>,</span> <span class='hs-varid'>cI</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-str'>"+"</span><span class='hs-layout'>,</span> <span class='hs-varid'>dOp2C</span> <span class='hs-str'>"+"</span> <span class='hs-layout'>(</span><span class='hs-varop'>+</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-str'>"*"</span><span class='hs-layout'>,</span> <span class='hs-varid'>dOp2C</span> <span class='hs-str'>"*"</span> <span class='hs-layout'>(</span><span class='hs-varop'>*</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span>
   <span class='hs-layout'>(</span><span class='hs-str'>"0"</span><span class='hs-layout'>,</span> <span class='hs-varid'>num2C</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-str'>"1"</span><span class='hs-layout'>,</span> <span class='hs-varid'>num2C</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span></pre>
Why not<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span>
  <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-str'>"I"</span><span class='hs-layout'>,</span> <span class='hs-varid'>cI</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-str'>"+"</span><span class='hs-layout'>,</span> <span class='hs-varid'>dOp2C</span> <span class='hs-str'>"+"</span> <span class='hs-layout'>(</span><span class='hs-varop'>+</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-str'>"*"</span><span class='hs-layout'>,</span> <span class='hs-varid'>dOp2C</span> <span class='hs-str'>"*"</span> <span class='hs-layout'>(</span><span class='hs-varop'>*</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span>
   <span class='hs-layout'>(</span><span class='hs-str'>"0"</span><span class='hs-layout'>,</span> <span class='hs-varid'>num2C</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-str'>"1"</span><span class='hs-layout'>,</span> <span class='hs-varid'>num2C</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span></pre>

</div>

<div class="hint18 file21">
src/Strappy/StdLib.hs:275:18: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-varop'>$</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-comment'>-</span><span class='hs-num'>3</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Double</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>elems</span> <span class='hs-varid'>stdlib'</span><span class='hs-keyglyph'>]</span></pre>
Why not<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-comment'>-</span><span class='hs-num'>3</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Double</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>elems</span> <span class='hs-varid'>stdlib'</span><span class='hs-keyglyph'>]</span></pre>

</div>

<div class="hint20 file21">
src/Strappy/StdLib.hs:275:33: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-comment'>-</span><span class='hs-num'>3</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Double</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-comment'>-</span><span class='hs-num'>3</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Double</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint18 file21">
src/Strappy/StdLib.hs:279:17: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-varop'>$</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>cI</span><span class='hs-layout'>,</span> <span class='hs-varid'>cTrue</span><span class='hs-layout'>,</span> <span class='hs-varid'>cFalse</span><span class='hs-layout'>,</span> <span class='hs-varid'>cAnd</span><span class='hs-layout'>,</span> <span class='hs-varid'>cOr</span><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span></pre>
Why not<br/>
<pre><span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>fromList</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>cI</span><span class='hs-layout'>,</span> <span class='hs-varid'>cTrue</span><span class='hs-layout'>,</span> <span class='hs-varid'>cFalse</span><span class='hs-layout'>,</span> <span class='hs-varid'>cAnd</span><span class='hs-layout'>,</span> <span class='hs-varid'>cOr</span><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span></pre>

</div>

<div class="hint20 file22">
src/Strappy/Task.hs:12:20: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-definition'>task</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Comb</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Reward</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>task</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Comb</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Reward</span></pre>

</div>

<div class="hint18 file22">
src/Strappy/Task.hs:34:17: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>fromIntegral</span> <span class='hs-varop'>$</span> <span class='hs-layout'>(</span><span class='hs-varid'>abs</span> <span class='hs-varop'>$</span> <span class='hs-varid'>a</span> <span class='hs-comment'>-</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>fromIntegral</span> <span class='hs-layout'>(</span><span class='hs-varid'>abs</span> <span class='hs-varop'>$</span> <span class='hs-varid'>a</span> <span class='hs-comment'>-</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint18 file22">
src/Strappy/Task.hs:34:17: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>fromIntegral</span> <span class='hs-varop'>$</span> <span class='hs-layout'>(</span><span class='hs-varid'>abs</span> <span class='hs-varop'>$</span> <span class='hs-varid'>a</span> <span class='hs-comment'>-</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>fromIntegral</span> <span class='hs-varop'>$</span> <span class='hs-varid'>abs</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-comment'>-</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint6 file23">
src/Strappy/Test.hs:2:1: Error: Unused LANGUAGE pragma<br/>
Found<br/>
<pre><span class='hs-comment'>{-# LANGUAGE ParallelListComp #-}</span></pre>
Why not remove it.<br/>
<pre></pre>

</div>

<div class="hint18 file23">
src/Strappy/Test.hs:27:11: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-str'>"OUT: "</span> <span class='hs-varop'>++</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varop'>$</span> <span class='hs-varid'>snd</span> <span class='hs-varid'>out</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>fst</span> <span class='hs-varid'>out</span></pre>
Why not<br/>
<pre><span class='hs-definition'>trace</span> <span class='hs-layout'>(</span><span class='hs-str'>"OUT: "</span> <span class='hs-varop'>++</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varop'>$</span> <span class='hs-varid'>snd</span> <span class='hs-varid'>out</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>fst</span> <span class='hs-varid'>out</span></pre>

</div>

<div class="hint18 file23">
src/Strappy/Test.hs:27:20: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-str'>"OUT: "</span> <span class='hs-varop'>++</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varop'>$</span> <span class='hs-varid'>snd</span> <span class='hs-varid'>out</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-str'>"OUT: "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-layout'>(</span><span class='hs-varid'>snd</span> <span class='hs-varid'>out</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint3 file23">
src/Strappy/Test.hs:31:8: Error: Redundant do<br/>
Found<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>saveSearchData</span> <span class='hs-str'>"data"</span> <span class='hs-varid'>expSymReg</span> <span class='hs-varid'>searchData</span></pre>
Why not<br/>
<pre><span class='hs-definition'>saveSearchData</span> <span class='hs-str'>"data"</span> <span class='hs-varid'>expSymReg</span> <span class='hs-varid'>searchData</span></pre>

</div>

<div class="hint6 file40">
src/Strappy/Type.hs:3:1: Error: Unused LANGUAGE pragma<br/>
Found<br/>
<pre><span class='hs-comment'>{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns,
  DeriveFunctor #-}</span></pre>
Why not<br/>
<pre><span class='hs-comment'>{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}</span></pre>

</div>

<div class="hint22 file40">
src/Strappy/Type.hs:73:12: Warning: Use :<br/>
Found<br/>
<pre><span class='hs-str'>"v"</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>n</span></pre>
Why not<br/>
<pre><span class='hs-chr'>'v'</span> <span class='hs-conop'>:</span> <span class='hs-varid'>show</span> <span class='hs-varid'>n</span></pre>

</div>

<div class="hint20 file40">
src/Strappy/Type.hs:110:22: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-layout'>(</span><span class='hs-varid'>kind</span> <span class='hs-varid'>t</span><span class='hs-layout'>)</span> <span class='hs-keyword'>of</span>
    <span class='hs-layout'>(</span><span class='hs-conid'>Kfun</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>k</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-varid'>kind</span> <span class='hs-varid'>t</span> <span class='hs-keyword'>of</span>
    <span class='hs-layout'>(</span><span class='hs-conid'>Kfun</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>k</span></pre>

</div>

<div class="hint14 file40">
src/Strappy/Type.hs:164:38: Warning: Avoid lambda<br/>
Found<br/>
<pre><span class='hs-keyglyph'>\</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>isTVar</span> <span class='hs-layout'>(</span><span class='hs-varid'>snd</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>isTVar</span> <span class='hs-varop'>.</span> <span class='hs-varid'>snd</span></pre>

</div>

<div class="hint25 file40">
src/Strappy/Type.hs:188:24: Warning: Use fromMaybe<br/>
Found<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-varid'>lookup</span> <span class='hs-varid'>u</span> <span class='hs-varid'>s</span> <span class='hs-keyword'>of</span>
    <span class='hs-conid'>Just</span> <span class='hs-varid'>t</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>t</span>
    <span class='hs-conid'>Nothing</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>TVar</span> <span class='hs-varid'>u</span></pre>
Why not<br/>
<pre><span class='hs-definition'>fromMaybe</span> <span class='hs-layout'>(</span><span class='hs-conid'>TVar</span> <span class='hs-varid'>u</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>lookup</span> <span class='hs-varid'>u</span> <span class='hs-varid'>s</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint8 file40">
src/Strappy/Type.hs:201:17: Error: Use concatMap<br/>
Found<br/>
<pre><span class='hs-definition'>concat</span> <span class='hs-varop'>.</span> <span class='hs-varid'>map</span> <span class='hs-varid'>tv</span></pre>
Why not<br/>
<pre><span class='hs-definition'>concatMap</span> <span class='hs-varid'>tv</span></pre>

</div>

<div class="hint0 file40">
src/Strappy/Type.hs:212:11: Error: Eta reduce<br/>
Found<br/>
<pre><span class='hs-definition'>dom</span> <span class='hs-varid'>s</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-varid'>fst</span> <span class='hs-varid'>s</span></pre>
Why not<br/>
<pre><span class='hs-definition'>dom</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>map</span> <span class='hs-varid'>fst</span></pre>

</div>

<div class="hint8 file40">
src/Strappy/Type.hs:254:16: Error: Use concatMap<br/>
Found<br/>
<pre><span class='hs-definition'>concat</span> <span class='hs-layout'>(</span><span class='hs-varid'>map</span> <span class='hs-varid'>tv</span> <span class='hs-varid'>tps</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>concatMap</span> <span class='hs-varid'>tv</span> <span class='hs-varid'>tps</span></pre>

</div>

<div class="hint7 file40">
src/Strappy/Type.hs:268:24: Error: Use .<br/>
Found<br/>
<pre><span class='hs-definition'>mapM</span> <span class='hs-varid'>newTVar</span> <span class='hs-layout'>(</span><span class='hs-varid'>map</span> <span class='hs-varid'>kind</span> <span class='hs-varid'>tvs</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>mapM</span> <span class='hs-layout'>(</span><span class='hs-varid'>newTVar</span> <span class='hs-varop'>.</span> <span class='hs-varid'>kind</span><span class='hs-layout'>)</span> <span class='hs-varid'>tvs</span></pre>

</div>

<div class="hint18 file40">
src/Strappy/Type.hs:289:25: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>return</span> <span class='hs-varop'>$</span> <span class='hs-varid'>t</span></pre>
Why not<br/>
<pre><span class='hs-definition'>return</span> <span class='hs-varid'>t</span></pre>

</div>

<div class="hint20 file41">
src/Strappy/Visualize.hs:59:48: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>newNode</span><span class='hs-layout'>,</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-layout'>(</span><span class='hs-varid'>labNodes</span> <span class='hs-varid'>gr</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>newNode</span><span class='hs-layout'>,</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>labNodes</span> <span class='hs-varid'>gr</span></pre>

</div>

<div class="hint18 file41">
src/Strappy/Visualize.hs:65:18: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-varid'>show</span> <span class='hs-varid'>nodes</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>mkGraph</span> <span class='hs-varid'>nodes</span> <span class='hs-varid'>edges</span></pre>
Why not<br/>
<pre><span class='hs-definition'>trace</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varid'>nodes</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>mkGraph</span> <span class='hs-varid'>nodes</span> <span class='hs-varid'>edges</span></pre>

</div>

<div class="hint20 file41">
src/Strappy/Visualize.hs:76:21: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-definition'>rootNode</span> <span class='hs-conop'>:</span> <span class='hs-layout'>(</span><span class='hs-varid'>labNodes</span> <span class='hs-varid'>withLeftRight</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>rootNode</span> <span class='hs-conop'>:</span> <span class='hs-varid'>labNodes</span> <span class='hs-varid'>withLeftRight</span></pre>

</div>

<div class="hint0 file41">
src/Strappy/Visualize.hs:79:1: Error: Eta reduce<br/>
Found<br/>
<pre><span class='hs-definition'>combsToGraph</span> <span class='hs-varid'>cs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>foldl</span> <span class='hs-layout'>(</span><span class='hs-varid'>addCombToGraph</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>mkGraph</span> <span class='hs-conid'>[]</span> <span class='hs-conid'>[]</span><span class='hs-layout'>)</span> <span class='hs-varid'>cs</span></pre>
Why not<br/>
<pre><span class='hs-definition'>combsToGraph</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>foldl</span> <span class='hs-layout'>(</span><span class='hs-varid'>addCombToGraph</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>mkGraph</span> <span class='hs-conid'>[]</span> <span class='hs-conid'>[]</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint2 file41">
src/Strappy/Visualize.hs:79:25: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>addCombToGraph</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>addCombToGraph</span></pre>

</div>

<div class="hint20 file24">
src/Strappy/Tests/AmbTI.hs:34:23: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>unpack</span> <span class='hs-varid'>m</span><span class='hs-layout'>)</span> <span class='hs-varop'>`mplus`</span> <span class='hs-layout'>(</span><span class='hs-varid'>concat</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>unpack</span> <span class='hs-varid'>m</span> <span class='hs-varop'>`mplus`</span> <span class='hs-layout'>(</span><span class='hs-varid'>concat</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file24">
src/Strappy/Tests/AmbTI.hs:34:23: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>unpack</span> <span class='hs-varid'>m</span><span class='hs-layout'>)</span> <span class='hs-varop'>`mplus`</span> <span class='hs-layout'>(</span><span class='hs-varid'>concat</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>unpack</span> <span class='hs-varid'>m</span><span class='hs-layout'>)</span> <span class='hs-varop'>`mplus`</span> <span class='hs-varid'>concat</span> <span class='hs-varid'>xs</span></pre>

</div>

<div class="hint0 file24">
src/Strappy/Tests/AmbTI.hs:53:5: Error: Eta reduce<br/>
Found<br/>
<pre><span class='hs-definition'>show</span> <span class='hs-varid'>m</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varop'>.</span> <span class='hs-varid'>toList</span><span class='hs-layout'>)</span> <span class='hs-varid'>m</span></pre>
Why not<br/>
<pre><span class='hs-definition'>show</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varop'>.</span> <span class='hs-varid'>toList</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file24">
src/Strappy/Tests/AmbTI.hs:57:24: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>runTI</span> <span class='hs-varid'>n</span> <span class='hs-conop'>:</span> <span class='hs-varid'>toList</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>runTI</span> <span class='hs-varid'>n</span> <span class='hs-conop'>:</span> <span class='hs-varid'>toList</span> <span class='hs-varid'>xs</span></pre>

</div>

<div class="hint5 file24">
src/Strappy/Tests/AmbTI.hs:70:8: Error: Redundant return<br/>
Found<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>i</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>get</span>
   <span class='hs-varid'>put</span> <span class='hs-layout'>(</span><span class='hs-varid'>i</span> <span class='hs-varop'>+</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span>
   <span class='hs-varid'>i'</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>get</span>
   <span class='hs-varid'>return</span> <span class='hs-varid'>i'</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>i</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>get</span>
   <span class='hs-varid'>put</span> <span class='hs-layout'>(</span><span class='hs-varid'>i</span> <span class='hs-varop'>+</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span>
   <span class='hs-varid'>get</span></pre>

</div>

<div class="hint20 file25">
src/Strappy/Tests/ListState.hs:14:12: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-layout'>(</span><span class='hs-varid'>runState</span> <span class='hs-layout'>(</span><span class='hs-varid'>runListT</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span> <span class='hs-keyword'>of</span>
    <span class='hs-layout'>(</span><span class='hs-conid'>Just</span> <span class='hs-varid'>y</span><span class='hs-layout'>,</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varid'>fst</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>toList</span> <span class='hs-layout'>(</span><span class='hs-varid'>snd</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
    <span class='hs-layout'>(</span><span class='hs-conid'>Nothing</span><span class='hs-layout'>,</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>[]</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-varid'>runState</span> <span class='hs-layout'>(</span><span class='hs-varid'>runListT</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-num'>0</span> <span class='hs-keyword'>of</span>
    <span class='hs-layout'>(</span><span class='hs-conid'>Just</span> <span class='hs-varid'>y</span><span class='hs-layout'>,</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varid'>fst</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>toList</span> <span class='hs-layout'>(</span><span class='hs-varid'>snd</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
    <span class='hs-layout'>(</span><span class='hs-conid'>Nothing</span><span class='hs-layout'>,</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>[]</span></pre>

</div>

<div class="hint20 file25">
src/Strappy/Tests/ListState.hs:14:12: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-layout'>(</span><span class='hs-varid'>runState</span> <span class='hs-layout'>(</span><span class='hs-varid'>runListT</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span> <span class='hs-keyword'>of</span>
    <span class='hs-layout'>(</span><span class='hs-conid'>Just</span> <span class='hs-varid'>y</span><span class='hs-layout'>,</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varid'>fst</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>toList</span> <span class='hs-layout'>(</span><span class='hs-varid'>snd</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
    <span class='hs-layout'>(</span><span class='hs-conid'>Nothing</span><span class='hs-layout'>,</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>[]</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-layout'>(</span><span class='hs-varid'>runState</span> <span class='hs-layout'>(</span><span class='hs-varid'>runListT</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span> <span class='hs-keyword'>of</span>
    <span class='hs-layout'>(</span><span class='hs-conid'>Just</span> <span class='hs-varid'>y</span><span class='hs-layout'>,</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>fst</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>toList</span> <span class='hs-layout'>(</span><span class='hs-varid'>snd</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span>
    <span class='hs-layout'>(</span><span class='hs-conid'>Nothing</span><span class='hs-layout'>,</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>[]</span></pre>

</div>

<div class="hint20 file25">
src/Strappy/Tests/ListState.hs:15:30: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>fst</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>toList</span> <span class='hs-layout'>(</span><span class='hs-varid'>snd</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>fst</span> <span class='hs-varid'>y</span> <span class='hs-conop'>:</span> <span class='hs-varid'>toList</span> <span class='hs-layout'>(</span><span class='hs-varid'>snd</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint30 file25">
src/Strappy/Tests/ListState.hs:25:19: Warning: Use string literal<br/>
Found<br/>
<pre><span class='hs-keyglyph'>[</span><span class='hs-chr'>'a'</span><span class='hs-layout'>,</span> <span class='hs-chr'>'b'</span><span class='hs-layout'>,</span> <span class='hs-chr'>'c'</span><span class='hs-keyglyph'>]</span></pre>
Why not<br/>
<pre><span class='hs-str'>"abc"</span></pre>

</div>

<div class="hint8 file26">
src/Strappy/Tests/TestBestFirst.hs:12:31: Error: Use concatMap<br/>
Found<br/>
<pre><span class='hs-definition'>concat</span> <span class='hs-varop'>$</span> <span class='hs-varid'>map</span> <span class='hs-layout'>(</span><span class='hs-varid'>toList</span> <span class='hs-varop'>.</span> <span class='hs-conid'>AOand</span><span class='hs-layout'>)</span> <span class='hs-varid'>ands</span></pre>
Why not<br/>
<pre><span class='hs-definition'>concatMap</span> <span class='hs-layout'>(</span><span class='hs-varid'>toList</span> <span class='hs-varop'>.</span> <span class='hs-conid'>AOand</span><span class='hs-layout'>)</span> <span class='hs-varid'>ands</span></pre>

</div>

<div class="hint16 file27">
src/Strappy/Tests/TestBranching.hs:98:15: Warning: Parse error<br/>
Error message<br/>
<pre>Parse error: Last statement in a do-block must be an expression</pre>
Code<br/>
<pre>                        <span class='hs-varid'>s</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>getSubst</span>
                        <span class='hs-varid'>n</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>return</span>
<span class='hs-varop'>&gt;</span>               <span class='hs-varid'>s</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>getSubst</span>
                <span class='hs-varid'>n</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>return</span> <span class='hs-varop'>$</span> <span class='hs-varid'>getVarInt</span>
                <span class='hs-layout'>(</span><span class='hs-conid'>TI</span> <span class='hs-varid'>f</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>return</span> <span class='hs-varop'>$</span> <span class='hs-varid'>newTVar</span> <span class='hs-conid'>Star</span>
</pre>
</div>

<div class="hint23 file28">
src/Strappy/Tests/TestCompress.hs:35:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>test_compress</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>testCompress</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint20 file29">
src/Strappy/Tests/TestCompressionSearch.hs:58:15: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>length</span> <span class='hs-varop'>.</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>keys</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>length</span> <span class='hs-varop'>.</span> <span class='hs-conid'>CM</span><span class='hs-varop'>.</span><span class='hs-varid'>keys</span></pre>

</div>

<div class="hint18 file29">
src/Strappy/Tests/TestCompressionSearch.hs:62:18: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>last</span> <span class='hs-varop'>$</span> <span class='hs-varid'>assignment</span> <span class='hs-varop'>$</span> <span class='hs-varid'>node1</span><span class='hs-layout'>)</span> <span class='hs-varop'>==</span> <span class='hs-layout'>(</span><span class='hs-varid'>last</span> <span class='hs-varop'>$</span> <span class='hs-varid'>assignment</span> <span class='hs-varop'>$</span> <span class='hs-varid'>node2</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>last</span> <span class='hs-varop'>$</span> <span class='hs-varid'>assignment</span> <span class='hs-varop'>$</span> <span class='hs-varid'>node1</span><span class='hs-layout'>)</span> <span class='hs-varop'>==</span> <span class='hs-varid'>last</span> <span class='hs-layout'>(</span><span class='hs-varid'>assignment</span> <span class='hs-varop'>$</span> <span class='hs-varid'>node2</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint18 file29">
src/Strappy/Tests/TestCompressionSearch.hs:62:18: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>last</span> <span class='hs-varop'>$</span> <span class='hs-varid'>assignment</span> <span class='hs-varop'>$</span> <span class='hs-varid'>node1</span><span class='hs-layout'>)</span> <span class='hs-varop'>==</span> <span class='hs-layout'>(</span><span class='hs-varid'>last</span> <span class='hs-varop'>$</span> <span class='hs-varid'>assignment</span> <span class='hs-varop'>$</span> <span class='hs-varid'>node2</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>last</span> <span class='hs-layout'>(</span><span class='hs-varid'>assignment</span> <span class='hs-varop'>$</span> <span class='hs-varid'>node1</span><span class='hs-layout'>)</span> <span class='hs-varop'>==</span> <span class='hs-layout'>(</span><span class='hs-varid'>last</span> <span class='hs-varop'>$</span> <span class='hs-varid'>assignment</span> <span class='hs-varop'>$</span> <span class='hs-varid'>node2</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint18 file29">
src/Strappy/Tests/TestCompressionSearch.hs:62:26: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>assignment</span> <span class='hs-varop'>$</span> <span class='hs-varid'>node1</span></pre>
Why not<br/>
<pre><span class='hs-definition'>assignment</span> <span class='hs-varid'>node1</span></pre>

</div>

<div class="hint18 file29">
src/Strappy/Tests/TestCompressionSearch.hs:62:57: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>assignment</span> <span class='hs-varop'>$</span> <span class='hs-varid'>node2</span></pre>
Why not<br/>
<pre><span class='hs-definition'>assignment</span> <span class='hs-varid'>node2</span></pre>

</div>

<div class="hint23 file30">
src/Strappy/Tests/TestEnum.hs:29:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>t_left1</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>tLeft1</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint23 file30">
src/Strappy/Tests/TestEnum.hs:30:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>t_right0</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>tRight0</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint23 file31">
src/Strappy/Tests/TestGrammar.hs:45:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>test_estimateGrammar</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>testEstimateGrammar</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint23 file31">
src/Strappy/Tests/TestGrammar.hs:52:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>test_bernLogProb1</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>testBernLogProb1</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint2 file31">
src/Strappy/Tests/TestGrammar.hs:52:47: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-num'>0.5</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-num'>0.5</span></pre>

</div>

<div class="hint23 file31">
src/Strappy/Tests/TestGrammar.hs:53:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>test_bernLogProb2</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>testBernLogProb2</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint23 file31">
src/Strappy/Tests/TestGrammar.hs:55:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>test_calcLogProb1</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>testCalcLogProb1</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint20 file31">
src/Strappy/Tests/TestGrammar.hs:57:30: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>c8</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-comment'>-</span><span class='hs-num'>2</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>c8</span><span class='hs-layout'>,</span> <span class='hs-comment'>-</span><span class='hs-num'>2</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file31">
src/Strappy/Tests/TestGrammar.hs:57:42: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>c9</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-comment'>-</span><span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>c9</span><span class='hs-layout'>,</span> <span class='hs-comment'>-</span><span class='hs-num'>1</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file31">
src/Strappy/Tests/TestGrammar.hs:58:16: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-comment'>-</span><span class='hs-num'>3</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-comment'>-</span><span class='hs-num'>3</span></pre>

</div>

<div class="hint20 file32">
src/Strappy/Tests/TestHashMap.hs:17:6: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>Map</span> <span class='hs-conid'>A</span> <span class='hs-conid'>Int</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span></pre>
Why not<br/>
<pre><span class='hs-conid'>Map</span> <span class='hs-conid'>A</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span></pre>

</div>

<div class="hint20 file32">
src/Strappy/Tests/TestHashMap.hs:19:19: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>g</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-varop'>+</span> <span class='hs-layout'>(</span><span class='hs-varid'>f</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>g</span> <span class='hs-varid'>xs</span> <span class='hs-varop'>+</span> <span class='hs-layout'>(</span><span class='hs-varid'>f</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file32">
src/Strappy/Tests/TestHashMap.hs:19:19: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>g</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-varop'>+</span> <span class='hs-layout'>(</span><span class='hs-varid'>f</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>g</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-varop'>+</span> <span class='hs-varid'>f</span> <span class='hs-varid'>xs</span></pre>

</div>

<div class="hint5 file39">
src/Strappy/Tests/TestingLogict.hs:11:8: Error: Redundant return<br/>
Found<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>i</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>get</span>
   <span class='hs-varid'>put</span> <span class='hs-layout'>(</span><span class='hs-varid'>i</span> <span class='hs-varop'>+</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span>
   <span class='hs-varid'>i'</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>get</span>
   <span class='hs-varid'>return</span> <span class='hs-varid'>i'</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>i</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>get</span>
   <span class='hs-varid'>put</span> <span class='hs-layout'>(</span><span class='hs-varid'>i</span> <span class='hs-varop'>+</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span>
   <span class='hs-varid'>get</span></pre>

</div>

<div class="hint3 file39">
src/Strappy/Tests/TestingLogict.hs:19:8: Error: Redundant do<br/>
Found<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>putStrLn</span> <span class='hs-varop'>$</span> <span class='hs-varid'>show</span> <span class='hs-layout'>(</span><span class='hs-varid'>fst</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runState</span> <span class='hs-layout'>(</span><span class='hs-varid'>observeAllT</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>putStrLn</span> <span class='hs-varop'>$</span> <span class='hs-varid'>show</span> <span class='hs-layout'>(</span><span class='hs-varid'>fst</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runState</span> <span class='hs-layout'>(</span><span class='hs-varid'>observeAllT</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint12 file39">
src/Strappy/Tests/TestingLogict.hs:20:3: Error: Use print<br/>
Found<br/>
<pre><span class='hs-definition'>putStrLn</span> <span class='hs-varop'>$</span> <span class='hs-varid'>show</span> <span class='hs-layout'>(</span><span class='hs-varid'>fst</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runState</span> <span class='hs-layout'>(</span><span class='hs-varid'>observeAllT</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>print</span> <span class='hs-layout'>(</span><span class='hs-varid'>fst</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runState</span> <span class='hs-layout'>(</span><span class='hs-varid'>observeAllT</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint10 file33">
src/Strappy/Tests/TestListTI.hs:12:1: Error: Use fewer imports<br/>
Found<br/>
<pre><span class='hs-keyword'>import</span> <span class='hs-conid'>StdLib</span> <span class='hs-layout'>(</span><span class='hs-varid'>stdlib</span><span class='hs-layout'>)</span>
<span class='hs-keyword'>import</span> <span class='hs-conid'>StdLib</span>
</pre>
Why not<br/>
<pre><span class='hs-keyword'>import</span> <span class='hs-conid'>StdLib</span>
</pre>

</div>

<div class="hint20 file33">
src/Strappy/Tests/TestListTI.hs:94:22: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>AmbTI</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-varop'>&gt;&gt;=</span> <span class='hs-varid'>k</span></pre>
Why not<br/>
<pre><span class='hs-conid'>AmbTI</span> <span class='hs-varid'>xs</span> <span class='hs-varop'>&gt;&gt;=</span> <span class='hs-varid'>k</span></pre>

</div>

<div class="hint20 file33">
src/Strappy/Tests/TestListTI.hs:96:24: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-conid'>AmbTI</span> <span class='hs-conid'>[]</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-conid'>AmbTI</span> <span class='hs-conid'>[]</span></pre>

</div>

<div class="hint18 file33">
src/Strappy/Tests/TestListTI.hs:110:18: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>liftTI</span> <span class='hs-varop'>$</span> <span class='hs-varid'>getVarInt</span></pre>
Why not<br/>
<pre><span class='hs-definition'>liftTI</span> <span class='hs-varid'>getVarInt</span></pre>

</div>

<div class="hint18 file33">
src/Strappy/Tests/TestListTI.hs:115:18: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>liftTI</span> <span class='hs-varop'>$</span> <span class='hs-varid'>getSubst</span></pre>
Why not<br/>
<pre><span class='hs-definition'>liftTI</span> <span class='hs-varid'>getSubst</span></pre>

</div>

<div class="hint18 file33">
src/Strappy/Tests/TestListTI.hs:116:13: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span>
   <span class='hs-str'>"\n INFO: "</span> <span class='hs-varop'>++</span>
     <span class='hs-str'>"\n depth: "</span> <span class='hs-varop'>++</span>
       <span class='hs-varid'>show</span> <span class='hs-varid'>d</span> <span class='hs-varop'>++</span>
         <span class='hs-str'>"\n varInt: "</span> <span class='hs-varop'>++</span>
           <span class='hs-varid'>show</span> <span class='hs-varid'>v</span> <span class='hs-varop'>++</span>
             <span class='hs-str'>"\n sub: "</span> <span class='hs-varop'>++</span>
               <span class='hs-varid'>show</span> <span class='hs-varid'>s</span> <span class='hs-varop'>++</span>
                 <span class='hs-str'>"\n comb a: "</span> <span class='hs-varop'>++</span>
                   <span class='hs-varid'>show</span> <span class='hs-varid'>a</span> <span class='hs-varop'>++</span>
                     <span class='hs-str'>"\n comb b: "</span> <span class='hs-varop'>++</span>
                       <span class='hs-varid'>show</span> <span class='hs-varid'>b</span> <span class='hs-varop'>++</span>
                         <span class='hs-str'>"\n inType: "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-layout'>(</span><span class='hs-varid'>tp</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t</span><span class='hs-layout'>)</span> <span class='hs-varop'>++</span> <span class='hs-str'>"\n outType: "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-layout'>(</span><span class='hs-varid'>aType</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
  <span class='hs-varop'>$</span> <span class='hs-varid'>liftTI</span> <span class='hs-varop'>$</span> <span class='hs-varid'>typeCheck</span> <span class='hs-varid'>c</span></pre>
Why not<br/>
<pre><span class='hs-definition'>trace</span>
  <span class='hs-layout'>(</span><span class='hs-str'>"\n INFO: "</span> <span class='hs-varop'>++</span>
     <span class='hs-str'>"\n depth: "</span> <span class='hs-varop'>++</span>
       <span class='hs-varid'>show</span> <span class='hs-varid'>d</span> <span class='hs-varop'>++</span>
         <span class='hs-str'>"\n varInt: "</span> <span class='hs-varop'>++</span>
           <span class='hs-varid'>show</span> <span class='hs-varid'>v</span> <span class='hs-varop'>++</span>
             <span class='hs-str'>"\n sub: "</span> <span class='hs-varop'>++</span>
               <span class='hs-varid'>show</span> <span class='hs-varid'>s</span> <span class='hs-varop'>++</span>
                 <span class='hs-str'>"\n comb a: "</span> <span class='hs-varop'>++</span>
                   <span class='hs-varid'>show</span> <span class='hs-varid'>a</span> <span class='hs-varop'>++</span>
                     <span class='hs-str'>"\n comb b: "</span> <span class='hs-varop'>++</span>
                       <span class='hs-varid'>show</span> <span class='hs-varid'>b</span> <span class='hs-varop'>++</span>
                         <span class='hs-str'>"\n inType: "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-layout'>(</span><span class='hs-varid'>tp</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>t</span><span class='hs-layout'>)</span> <span class='hs-varop'>++</span> <span class='hs-str'>"\n outType: "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-layout'>(</span><span class='hs-varid'>aType</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
  <span class='hs-varop'>$</span> <span class='hs-varid'>liftTI</span> <span class='hs-varop'>$</span> <span class='hs-varid'>typeCheck</span> <span class='hs-varid'>c</span></pre>

</div>

<div class="hint2 file33">
src/Strappy/Tests/TestListTI.hs:124:37: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>aType</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>aType</span></pre>

</div>

<div class="hint18 file33">
src/Strappy/Tests/TestListTI.hs:134:15: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>liftTI</span> <span class='hs-varop'>$</span> <span class='hs-varid'>getVarInt</span></pre>
Why not<br/>
<pre><span class='hs-definition'>liftTI</span> <span class='hs-varid'>getVarInt</span></pre>

</div>

<div class="hint18 file33">
src/Strappy/Tests/TestListTI.hs:135:19: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>liftTI</span> <span class='hs-varop'>$</span> <span class='hs-varid'>getSubst</span></pre>
Why not<br/>
<pre><span class='hs-definition'>liftTI</span> <span class='hs-varid'>getSubst</span></pre>

</div>

<div class="hint18 file33">
src/Strappy/Tests/TestListTI.hs:136:18: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-varid'>show</span> <span class='hs-varid'>t</span> <span class='hs-varop'>++</span> <span class='hs-str'>" "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>tc</span> <span class='hs-varop'>++</span> <span class='hs-str'>" "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>subst</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span>
  <span class='hs-varid'>liftTI</span> <span class='hs-varop'>$</span> <span class='hs-varid'>unify'</span> <span class='hs-varid'>tc</span> <span class='hs-varid'>t</span></pre>
Why not<br/>
<pre><span class='hs-definition'>trace</span> <span class='hs-layout'>(</span><span class='hs-varid'>show</span> <span class='hs-varid'>t</span> <span class='hs-varop'>++</span> <span class='hs-str'>" "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>tc</span> <span class='hs-varop'>++</span> <span class='hs-str'>" "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>subst</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span>
  <span class='hs-varid'>liftTI</span> <span class='hs-varop'>$</span> <span class='hs-varid'>unify'</span> <span class='hs-varid'>tc</span> <span class='hs-varid'>t</span></pre>

</div>

<div class="hint18 file33">
src/Strappy/Tests/TestListTI.hs:138:10: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>trace</span> <span class='hs-varop'>$</span> <span class='hs-str'>"succ : "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>succ</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>return</span> <span class='hs-varid'>c</span></pre>
Why not<br/>
<pre><span class='hs-definition'>trace</span> <span class='hs-layout'>(</span><span class='hs-str'>"succ : "</span> <span class='hs-varop'>++</span> <span class='hs-varid'>show</span> <span class='hs-varid'>succ</span><span class='hs-layout'>)</span> <span class='hs-varop'>$</span> <span class='hs-varid'>return</span> <span class='hs-varid'>c</span></pre>

</div>

<div class="hint20 file33">
src/Strappy/Tests/TestListTI.hs:142:25: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-varid'>runTISafe</span> <span class='hs-varid'>x</span> <span class='hs-keyword'>of</span>
    <span class='hs-conid'>Right</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-conop'>:</span> <span class='hs-varid'>rest</span><span class='hs-layout'>)</span>
    <span class='hs-conid'>Left</span> <span class='hs-varid'>err</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>rest</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-keyword'>case</span> <span class='hs-varid'>runTISafe</span> <span class='hs-varid'>x</span> <span class='hs-keyword'>of</span>
    <span class='hs-conid'>Right</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-conop'>:</span> <span class='hs-varid'>rest</span>
    <span class='hs-conid'>Left</span> <span class='hs-varid'>err</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>rest</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint2 file33">
src/Strappy/Tests/TestListTI.hs:144:37: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>rest</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>rest</span></pre>

</div>

<div class="hint20 file33">
src/Strappy/Tests/TestListTI.hs:152:5: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>newTVar</span> <span class='hs-conid'>Star</span><span class='hs-layout'>)</span> <span class='hs-varop'>&gt;&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>return</span> <span class='hs-varop'>$</span> <span class='hs-conid'>AmbTI</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>b</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>newTVar</span> <span class='hs-conid'>Star</span> <span class='hs-varop'>&gt;&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>return</span> <span class='hs-varop'>$</span> <span class='hs-conid'>AmbTI</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>b</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint18 file33">
src/Strappy/Tests/TestListTI.hs:152:5: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>newTVar</span> <span class='hs-conid'>Star</span><span class='hs-layout'>)</span> <span class='hs-varop'>&gt;&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>return</span> <span class='hs-varop'>$</span> <span class='hs-conid'>AmbTI</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>b</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>newTVar</span> <span class='hs-conid'>Star</span><span class='hs-layout'>)</span> <span class='hs-varop'>&gt;&gt;</span> <span class='hs-varid'>return</span> <span class='hs-layout'>(</span><span class='hs-conid'>AmbTI</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>b</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file34">
src/Strappy/Tests/TestStateT.hs:25:18: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-definition'>runTypeInfT</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Subst</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>TIError</span> <span class='hs-varid'>m</span> <span class='hs-layout'>(</span><span class='hs-conid'>Subst</span><span class='hs-layout'>,</span> <span class='hs-conid'>Int</span><span class='hs-layout'>,</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>runTypeInfT</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Subst</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>TIError</span> <span class='hs-varid'>m</span> <span class='hs-layout'>(</span><span class='hs-conid'>Subst</span><span class='hs-layout'>,</span> <span class='hs-conid'>Int</span><span class='hs-layout'>,</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint15 file34">
src/Strappy/Tests/TestStateT.hs:30:26: Warning: Collapse lambdas<br/>
Found<br/>
<pre><span class='hs-keyglyph'>\</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>\</span> <span class='hs-varid'>i</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>lift</span> <span class='hs-varop'>.</span> <span class='hs-varid'>return</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-varid'>i</span><span class='hs-layout'>,</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-keyglyph'>\</span> <span class='hs-varid'>c</span> <span class='hs-varid'>i</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>lift</span> <span class='hs-varop'>.</span> <span class='hs-varid'>return</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-layout'>,</span> <span class='hs-varid'>i</span><span class='hs-layout'>,</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint15 file34">
src/Strappy/Tests/TestStateT.hs:31:27: Warning: Collapse lambdas<br/>
Found<br/>
<pre><span class='hs-keyglyph'>\</span> <span class='hs-varid'>s</span> <span class='hs-keyglyph'>-&gt;</span>
  <span class='hs-keyglyph'>\</span> <span class='hs-varid'>i</span> <span class='hs-keyglyph'>-&gt;</span>
    <span class='hs-keyword'>do</span> <span class='hs-layout'>(</span><span class='hs-varid'>s'</span><span class='hs-layout'>,</span> <span class='hs-varid'>i'</span><span class='hs-layout'>,</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>runTypeInfT</span> <span class='hs-varid'>m</span> <span class='hs-varid'>s</span> <span class='hs-varid'>i</span>
       <span class='hs-varid'>runTypeInfT</span> <span class='hs-layout'>(</span><span class='hs-varid'>f</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-varid'>s'</span> <span class='hs-varid'>i'</span></pre>
Why not<br/>
<pre><span class='hs-keyglyph'>\</span> <span class='hs-varid'>s</span> <span class='hs-varid'>i</span> <span class='hs-keyglyph'>-&gt;</span>
  <span class='hs-keyword'>do</span> <span class='hs-layout'>(</span><span class='hs-varid'>s'</span><span class='hs-layout'>,</span> <span class='hs-varid'>i'</span><span class='hs-layout'>,</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>runTypeInfT</span> <span class='hs-varid'>m</span> <span class='hs-varid'>s</span> <span class='hs-varid'>i</span>
     <span class='hs-varid'>runTypeInfT</span> <span class='hs-layout'>(</span><span class='hs-varid'>f</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-varid'>s'</span> <span class='hs-varid'>i'</span></pre>

</div>

<div class="hint15 file34">
src/Strappy/Tests/TestStateT.hs:37:25: Warning: Collapse lambdas<br/>
Found<br/>
<pre><span class='hs-keyglyph'>\</span> <span class='hs-varid'>s</span> <span class='hs-keyglyph'>-&gt;</span>
  <span class='hs-keyglyph'>\</span> <span class='hs-varid'>i</span> <span class='hs-keyglyph'>-&gt;</span>
    <span class='hs-varid'>lift</span> <span class='hs-varop'>$</span>
      <span class='hs-keyword'>do</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>m</span>
         <span class='hs-varid'>return</span> <span class='hs-layout'>(</span><span class='hs-varid'>s</span><span class='hs-layout'>,</span> <span class='hs-varid'>i</span><span class='hs-layout'>,</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-keyglyph'>\</span> <span class='hs-varid'>s</span> <span class='hs-varid'>i</span> <span class='hs-keyglyph'>-&gt;</span>
  <span class='hs-varid'>lift</span> <span class='hs-varop'>$</span>
    <span class='hs-keyword'>do</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>m</span>
       <span class='hs-varid'>return</span> <span class='hs-layout'>(</span><span class='hs-varid'>s</span><span class='hs-layout'>,</span> <span class='hs-varid'>i</span><span class='hs-layout'>,</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint15 file34">
src/Strappy/Tests/TestStateT.hs:42:24: Warning: Collapse lambdas<br/>
Found<br/>
<pre><span class='hs-keyglyph'>\</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>\</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>lift</span> <span class='hs-varop'>$</span> <span class='hs-varid'>mzero</span></pre>
Why not<br/>
<pre><span class='hs-keyglyph'>\</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>lift</span> <span class='hs-varop'>$</span> <span class='hs-varid'>mzero</span></pre>

</div>

<div class="hint18 file34">
src/Strappy/Tests/TestStateT.hs:42:37: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>lift</span> <span class='hs-varop'>$</span> <span class='hs-varid'>mzero</span></pre>
Why not<br/>
<pre><span class='hs-definition'>lift</span> <span class='hs-varid'>mzero</span></pre>

</div>

<div class="hint20 file34">
src/Strappy/Tests/TestStateT.hs:56:36: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>s</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-varid'>i</span> <span class='hs-varop'>+</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-conid'>Type</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>s</span><span class='hs-layout'>,</span> <span class='hs-varid'>i</span> <span class='hs-varop'>+</span> <span class='hs-num'>1</span><span class='hs-layout'>,</span> <span class='hs-conid'>Type</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint2 file34">
src/Strappy/Tests/TestStateT.hs:66:44: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>newTVar</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>newTVar</span></pre>

</div>

<div class="hint3 file35">
src/Strappy/Tests/TestTrans.hs:42:8: Error: Redundant do<br/>
Found<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>print</span> <span class='hs-varop'>$</span> <span class='hs-layout'>(</span><span class='hs-varid'>runErrorT</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>print</span> <span class='hs-varop'>$</span> <span class='hs-layout'>(</span><span class='hs-varid'>runErrorT</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint18 file35">
src/Strappy/Tests/TestTrans.hs:43:3: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>print</span> <span class='hs-varop'>$</span> <span class='hs-layout'>(</span><span class='hs-varid'>runErrorT</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>print</span> <span class='hs-layout'>(</span><span class='hs-varid'>runErrorT</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint18 file35">
src/Strappy/Tests/TestTrans.hs:43:3: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>print</span> <span class='hs-varop'>$</span> <span class='hs-layout'>(</span><span class='hs-varid'>runErrorT</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>print</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runErrorT</span> <span class='hs-layout'>(</span><span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file36">
src/Strappy/Tests/TestTrans2.hs:22:29: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>m</span><span class='hs-layout'>)</span> <span class='hs-varop'>`mplus`</span> <span class='hs-layout'>(</span><span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>n</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>runMyTrans</span> <span class='hs-varid'>m</span> <span class='hs-varop'>`mplus`</span> <span class='hs-layout'>(</span><span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>n</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint20 file36">
src/Strappy/Tests/TestTrans2.hs:22:29: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>m</span><span class='hs-layout'>)</span> <span class='hs-varop'>`mplus`</span> <span class='hs-layout'>(</span><span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>n</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>m</span><span class='hs-layout'>)</span> <span class='hs-varop'>`mplus`</span> <span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>n</span></pre>

</div>

<div class="hint3 file36">
src/Strappy/Tests/TestTrans2.hs:42:8: Error: Redundant do<br/>
Found<br/>
<pre><span class='hs-keyword'>do</span> <span class='hs-varid'>print</span> <span class='hs-varop'>$</span> <span class='hs-layout'>(</span><span class='hs-varid'>runErrorT</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>print</span> <span class='hs-varop'>$</span> <span class='hs-layout'>(</span><span class='hs-varid'>runErrorT</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint18 file36">
src/Strappy/Tests/TestTrans2.hs:43:3: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>print</span> <span class='hs-varop'>$</span> <span class='hs-layout'>(</span><span class='hs-varid'>runErrorT</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>print</span> <span class='hs-layout'>(</span><span class='hs-varid'>runErrorT</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint18 file36">
src/Strappy/Tests/TestTrans2.hs:43:3: Warning: Redundant $<br/>
Found<br/>
<pre><span class='hs-definition'>print</span> <span class='hs-varop'>$</span> <span class='hs-layout'>(</span><span class='hs-varid'>runErrorT</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-definition'>print</span> <span class='hs-varop'>$</span> <span class='hs-varid'>runErrorT</span> <span class='hs-layout'>(</span><span class='hs-varid'>runMyTrans</span> <span class='hs-varid'>z</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint17 file37">
src/Strappy/Tests/TestType.hs:64:19: Warning: Reduce duplication<br/>
Found<br/>
<pre><span class='hs-definition'>t0</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>mkTVar</span> <span class='hs-num'>0</span>
<span class='hs-definition'>t1</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>mkTVar</span> <span class='hs-num'>1</span>
<span class='hs-definition'>tv0</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>TyVar</span> <span class='hs-layout'>(</span><span class='hs-varid'>enumId</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span> <span class='hs-conid'>Star</span>
<span class='hs-definition'>tv1</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>TyVar</span> <span class='hs-layout'>(</span><span class='hs-varid'>enumId</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span> <span class='hs-conid'>Star</span>
</pre>
Why not<br/>
<pre><span class='hs-conid'>Combine</span> <span class='hs-varid'>with</span> <span class='hs-varid'>src</span><span class='hs-varop'>/</span><span class='hs-conid'>Strappy</span><span class='hs-varop'>/</span><span class='hs-conid'>Tests</span><span class='hs-varop'>/</span><span class='hs-conid'>TestType</span><span class='hs-varop'>.</span><span class='hs-varid'>hs</span><span class='hs-conop'>:</span><span class='hs-num'>69</span><span class='hs-conop'>:</span><span class='hs-num'>19</span></pre>

</div>

<div class="hint20 file37">
src/Strappy/Tests/TestType.hs:55:25: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>apply</span> <span class='hs-varid'>nullSubst</span> <span class='hs-varid'>t</span><span class='hs-layout'>)</span> <span class='hs-varop'>==</span> <span class='hs-varid'>t</span></pre>
Why not<br/>
<pre><span class='hs-definition'>apply</span> <span class='hs-varid'>nullSubst</span> <span class='hs-varid'>t</span> <span class='hs-varop'>==</span> <span class='hs-varid'>t</span></pre>

</div>

<div class="hint23 file37">
src/Strappy/Tests/TestType.hs:60:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>test_mgu1</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>testMgu1</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint23 file37">
src/Strappy/Tests/TestType.hs:62:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>test_mgu2</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>testMgu2</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint23 file37">
src/Strappy/Tests/TestType.hs:63:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>test_mgu3</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>testMgu3</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint23 file37">
src/Strappy/Tests/TestType.hs:68:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>test_mgu4</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>testMgu4</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint23 file37">
src/Strappy/Tests/TestType.hs:74:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>test_mgu5</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>testMgu5</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint20 file37">
src/Strappy/Tests/TestType.hs:74:77: Warning: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>tv1</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-varid'>t2</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>tInt</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-layout'>(</span><span class='hs-varid'>tv1</span><span class='hs-layout'>,</span> <span class='hs-varid'>t2</span> <span class='hs-varop'>-&gt;-</span> <span class='hs-varid'>tInt</span><span class='hs-layout'>)</span></pre>

</div>

<div class="hint23 file37">
src/Strappy/Tests/TestType.hs:80:1: Warning: Use camelCase<br/>
Found<br/>
<pre><span class='hs-definition'>test_mgu6</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>
Why not<br/>
<pre><span class='hs-definition'>testMgu6</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span></pre>

</div>

<div class="hint2 file38">
src/Strappy/Tests/TestUtils.hs:18:33: Error: Redundant bracket<br/>
Found<br/>
<pre><span class='hs-layout'>(</span><span class='hs-num'>1.0e-6</span><span class='hs-layout'>)</span></pre>
Why not<br/>
<pre><span class='hs-num'>1.0e-6</span></pre>

</div>

</div>
</body>
</html>
