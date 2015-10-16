<!DOCTYPE html>
<html lang="en" dir="ltr">
<head>
<meta charset="UTF-8" />
<title>Runproof.sh - SRI SAL</title>
<meta name="generator" content="MediaWiki 1.17.0" />
<link rel="shortcut icon" href="/favicon.ico" />
<link rel="search" type="application/opensearchdescription+xml" href="/opensearch_desc.php" title="SRI SAL (en)" />
<link rel="EditURI" type="application/rsd+xml" href="http://sal-wiki.csl.sri.com/api.php?action=rsd" />
<link rel="copyright" href="http://www.gnu.org/copyleft/fdl.html" />
<link rel="alternate" type="application/atom+xml" title="SRI SAL Atom feed" href="/index.php?title=Special:RecentChanges&amp;feed=atom" />
<link rel="stylesheet" href="/load.php?debug=false&amp;lang=en&amp;modules=mediawiki.legacy.commonPrint%2Cshared&amp;only=styles&amp;skin=monobook&amp;*" />
<link rel="stylesheet" href="/skins/monobook/main.css?301" media="screen" />
<!--[if lt IE 5.5000]><link rel="stylesheet" href="/skins/monobook/IE50Fixes.css?301" media="screen" /><![endif]-->
<!--[if IE 5.5000]><link rel="stylesheet" href="/skins/monobook/IE55Fixes.css?301" media="screen" /><![endif]-->
<!--[if IE 6]><link rel="stylesheet" href="/skins/monobook/IE60Fixes.css?301" media="screen" /><![endif]-->
<!--[if IE 7]><link rel="stylesheet" href="/skins/monobook/IE70Fixes.css?301" media="screen" /><![endif]--><meta name="ResourceLoaderDynamicStyles" content="" />
</head>
<body class="mediawiki ltr ns-0 ns-subject page-Runproof_sh skin-monobook">
<div id="globalWrapper">
<div id="column-content"><div id="content">
	<a id="top"></a>
	
	<h1 id="firstHeading" class="firstHeading">Runproof.sh</h1>
	<div id="bodyContent">
		<div id="siteSub">From SRI SAL</div>
		<div id="contentSub"></div>
		<div id="jump-to-nav">Jump to: <a href="#column-one">navigation</a>, <a href="#searchInput">search</a></div>
		<!-- start content -->
<pre>
#!/bin/bash

# BSD License
# Copyright (c) 2008, Lee Pike (Galois, Inc.) leepike [at] galois.com
# 
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are met:
#  * Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#  * Neither the name of the &lt;organization&gt; nor the
#    names of its contributors may be used to endorse or promote products
#    derived from this software without specific prior written permission.
# 
#  THIS SOFTWARE IS PROVIDED BY &lt;copyright holder&gt; ``AS IS'' AND ANY
#  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
#  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
#  DISCLAIMED. IN NO EVENT SHALL &lt;copyright holder&gt; BE LIABLE FOR ANY
#  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
#  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
#  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
#  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


# --- USAGE ---
# Takes one of the following args: sal-bmc, sal-smc, and sal-emc and a SAL file
# and runs that over all the theorems/lemmas/obligations/claims in the file.

# -- EXAMPLE ---
# Example usage: runproof.sh sal-bmc file.sal
#         returns sal-bmc for all theorems (likewise for sal-smc or sal-emc).

# Takes one of sal-bmc, sal-smc, and sal-emc and a SAL file and runs that 
# over all the theorems/lemmas/obligations/claims in the file.

# Example usage: runproof.sh sal-bmc file.sal
#        returns sal-bmc for all (likewise for sal-smc or sal-emc).

if [ $#&#160;!= 2 ]; then
  echo &quot;usage $0 &lt;sal-bmc|sal-smc|sal-emc&gt; &lt;sal_file&gt;&quot;
  exit 0
fi

case $1 in
  sal-bmc | sal-smc | sal-emc)&#160;;;
  *) echo &quot;usage $0 &lt;sal-bmc|sal-smc|sal-emc&gt; &lt;sal_file&gt;&quot;
     exit 0
esac

if [&#160;! -e $2 ]; then
   echo &quot;$2 not valid file&quot;
   exit 0
fi

egrep -e &quot;(OBLIGATON|CLAIM|LEMMA|THEOREM)&quot; $2 | sed -n &quot;/.*/s/:.*//pg&quot; | env cmd=$1 file=$2 awk '{print &quot;echo proving &quot; $0 &quot;:&quot; &quot;\n&quot;  ENVIRON[&quot;cmd&quot;] &quot; &quot; ENVIRON[&quot;file&quot;] &quot; &quot; $0}' | bash

</pre>

<!-- 
NewPP limit report
Preprocessor node count: 4/1000000
Post-expand include size: 0/2097152 bytes
Template argument size: 0/2097152 bytes
Expensive parser function count: 0/100
-->

<!-- Saved in parser cache with key sal:pcache:idhash:1417-0!*!0!!*!*!edit=0 and timestamp 20151015021517 -->
<div class="printfooter">
Retrieved from "<a href="http://sal-wiki.csl.sri.com/index.php/Runproof.sh">http://sal-wiki.csl.sri.com/index.php/Runproof.sh</a>"</div>
		<div id='catlinks' class='catlinks catlinks-allhidden'></div>		<!-- end content -->
				<div class="visualClear"></div>
	</div>
</div></div>
<div id="column-one">
	<div id="p-cactions" class="portlet">
		<h5>Views</h5>
		<div class="pBody">
			<ul>
				 <li id="ca-nstab-main" class="selected"><a href="/index.php/Runproof.sh" title="View the content page [c]" accesskey="c">Page</a></li>
				 <li id="ca-talk" class="new"><a href="/index.php?title=Talk:Runproof.sh&amp;action=edit&amp;redlink=1" title="Discussion about the content page [t]" accesskey="t">Discussion</a></li>
				 <li id="ca-viewsource"><a href="/index.php?title=Runproof.sh&amp;action=edit" title="This page is protected.&#10;You can view its source [e]" accesskey="e">View source</a></li>
				 <li id="ca-history"><a href="/index.php?title=Runproof.sh&amp;action=history" title="Past revisions of this page [h]" accesskey="h">History</a></li>
			</ul>
		</div>
	</div>
	<div class="portlet" id="p-personal">
		<h5>Personal tools</h5>
		<div class="pBody">
			<ul>
				<li id="pt-login"><a href="/index.php?title=Special:UserLogin&amp;returnto=Runproof.sh" title="You are encouraged to log in; however, it is not mandatory [o]" accesskey="o">Log in / create account</a></li>
			</ul>
		</div>
	</div>
	<div class="portlet" id="p-logo">
		<a style="background-image: url(/skins/common/images/sallogo-icon.png);" href="/index.php/Main_Page" title="Visit the main page"></a>
	</div>
	<script type="text/javascript"> if (window.isMSIE55) fixalpha(); </script>
	<div class='generated-sidebar portlet' id='p-navigation'>
		<h5>Navigation</h5>
		<div class='pBody'>
			<ul>
				<li id="n-SAL-Wiki-Main-Page"><a href="/index.php/Main_Page">SAL Wiki Main Page</a></li>
				<li id="n-SAL-Web-Page"><a href="http://sal.csl.sri.com">SAL Web Page</a></li>
				<li id="n-portal"><a href="/index.php/SRI_SAL:Community_portal" title="About the project, what you can do, where to find things">Community portal</a></li>
				<li id="n-currentevents"><a href="/index.php/SRI_SAL:Current_events" title="Find background information on current events">Current events</a></li>
				<li id="n-recentchanges"><a href="/index.php/Special:RecentChanges" title="The list of recent changes in the wiki [r]" accesskey="r">Recent changes</a></li>
				<li id="n-randompage"><a href="/index.php/Special:Random" title="Load a random page [x]" accesskey="x">Random page</a></li>
				<li id="n-help"><a href="/index.php/Help:Contents" title="The place to find out">Help</a></li>
			</ul>
		</div>
	</div>
	<div id="p-search" class="portlet">
		<h5><label for="searchInput">Search</label></h5>
		<div id="searchBody" class="pBody">
			<form action="/index.php" id="searchform">
				<input type='hidden' name="title" value="Special:Search"/>
				<input id="searchInput" title="Search SRI SAL" accesskey="f" type="search" name="search" />
				<input type='submit' name="go" class="searchButton" id="searchGoButton"	value="Go" title="Go to a page with this exact name if exists" />&#160;
				<input type='submit' name="fulltext" class="searchButton" id="mw-searchButton" value="Search" title="Search the pages for this text" />
			</form>
		</div>
	</div>
	<div class="portlet" id="p-tb">
		<h5>Toolbox</h5>
		<div class="pBody">
			<ul>
				<li id="t-whatlinkshere"><a href="/index.php/Special:WhatLinksHere/Runproof.sh" title="List of all wiki pages that link here [j]" accesskey="j">What links here</a></li>
				<li id="t-recentchangeslinked"><a href="/index.php/Special:RecentChangesLinked/Runproof.sh" title="Recent changes in pages linked from this page [k]" accesskey="k">Related changes</a></li>
<li id="t-specialpages"><a href="/index.php/Special:SpecialPages" title="List of all special pages [q]" accesskey="q">Special pages</a></li>
				<li id="t-print"><a href="/index.php?title=Runproof.sh&amp;printable=yes" rel="alternate" title="Printable version of this page [p]" accesskey="p">Printable version</a></li>				<li id="t-permalink"><a href="/index.php?title=Runproof.sh&amp;oldid=1563" title="Permanent link to this revision of the page">Permanent link</a></li>			</ul>
		</div>
	</div>
</div><!-- end of the left (by default at least) column -->
<div class="visualClear"></div>
<div id="footer">
	<div id="f-copyrightico">
		<a href="http://www.gnu.org/copyleft/fdl.html"><img src="" alt="GNU Free Documentation License" width="88" height="31" /></a>
	</div>
	<div id="f-poweredbyico">
		<a href="http://www.mediawiki.org/"><img src="/skins/common/images/poweredby_mediawiki_88x31.png" alt="Powered by MediaWiki" width="88" height="31" /></a>
	</div>
	<ul id="f-list">
		<li id="lastmod"> This page was last modified on 16 January 2008, at 07:25.</li>
		<li id="viewcount">This page has been accessed 195,869 times.</li>
		<li id="copyright">Content is available under <a href="http://www.gnu.org/copyleft/fdl.html" class="external ">GNU Free Documentation License</a>.</li>
		<li id="privacy"><a href="/index.php/SRI_SAL:Privacy_policy" title="SRI SAL:Privacy policy">Privacy policy</a></li>
		<li id="about"><a href="/index.php/SRI_SAL:About" title="SRI SAL:About">About SRI SAL</a></li>
		<li id="disclaimer"><a href="/index.php/SRI_SAL:General_disclaimer" title="SRI SAL:General disclaimer">Disclaimers</a></li>
	</ul>
</div>
</div>

<script src="/load.php?debug=false&amp;lang=en&amp;modules=startup&amp;only=scripts&amp;skin=monobook&amp;*"></script>
<script>if ( window.mediaWiki ) {
	mediaWiki.config.set({"wgCanonicalNamespace": "", "wgCanonicalSpecialPageName": false, "wgNamespaceNumber": 0, "wgPageName": "Runproof.sh", "wgTitle": "Runproof.sh", "wgAction": "view", "wgArticleId": 1417, "wgIsArticle": true, "wgUserName": null, "wgUserGroups": ["*"], "wgCurRevisionId": 1563, "wgCategories": [], "wgBreakFrames": false, "wgRestrictionEdit": [], "wgRestrictionMove": []});
}
</script>
<script>if ( window.mediaWiki ) {
	mediaWiki.loader.load(["mediawiki.util", "mediawiki.legacy.wikibits", "mediawiki.legacy.ajax"]);
	mediaWiki.loader.go();
}
</script>

<script>if ( window.mediaWiki ) {
	mediaWiki.user.options.set({"ccmeonemails":0,"cols":80,"contextchars":50,"contextlines":5,"date":"default","diffonly":0,"disablemail":0,"disablesuggest":0,"editfont":"default","editondblclick":0,"editsection":1,"editsectiononrightclick":0,"enotifminoredits":0,"enotifrevealaddr":0,"enotifusertalkpages":1,"enotifwatchlistpages":0,"extendwatchlist":0,"externaldiff":0,"externaleditor":0,"fancysig":0,"forceeditsummary":0,"gender":"unknown","hideminor":0,"hidepatrolled":0,"highlightbroken":1,"imagesize":2,"justify":0,"math":1,"minordefault":0,"newpageshidepatrolled":0,"nocache":0,"noconvertlink":0,"norollbackdiff":0,"numberheadings":0,"previewonfirst":0,"previewontop":1,"quickbar":1,"rcdays":7,"rclimit":50,"rememberpassword":0,"rows":25,"searchlimit":20,"showhiddencats":0,"showjumplinks":1,"shownumberswatching":1,"showtoc":1,"showtoolbar":1,"skin":"monobook","stubthreshold":0,"thumbsize":2,"underline":2,"uselivepreview":0,"usenewrc":0,"watchcreations":0,"watchdefault":0,"watchdeletion":0,
	"watchlistdays":3,"watchlisthideanons":0,"watchlisthidebots":0,"watchlisthideliu":0,"watchlisthideminor":0,"watchlisthideown":0,"watchlisthidepatrolled":0,"watchmoves":0,"wllimit":250,"variant":"en","language":"en","searchNs0":true,"searchNs1":false,"searchNs2":false,"searchNs3":false,"searchNs4":false,"searchNs5":false,"searchNs6":false,"searchNs7":false,"searchNs8":false,"searchNs9":false,"searchNs10":false,"searchNs11":false,"searchNs12":false,"searchNs13":false,"searchNs14":false,"searchNs15":false});;mediaWiki.loader.state({"user.options":"ready"});
}
</script><!-- Served in 0.574 secs. --></body></html>