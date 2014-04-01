@(vvh: VecViewHelper, decomposition: Html, recognized: Map[Int, (com.faacets.comp.PathLeaf, com.faacets.BellExpression)], other: Map[Int, VecViewHelper])

@scripts = {
<script type="text/javascript" src="@routes.Assets.at("javascripts/shCore.js")"></script>
<script type="text/javascript" src="@routes.Assets.at("javascripts/shBrushYaml.js")"></script>
<script type="text/javascript">SyntaxHighlighter.all();</script>
}

@main("Inequality decomposition", scripts) {
<div class="row" style="margin-top:20px"></div>

<h3>We decomposed your inequality, and it contains @(recognized.size) inequality/ies in the database and @(other.size) inequality/ies not yet in the database.</h3>

<ul class="nav nav-tabs">
  <li class="active"><a href="#inequality" data-toggle="tab">Your inequality</a></li>
  <li><a href="#decomposition" data-toggle="tab">The decomposition</a></li>
  @for( (i, entry) <- recognized.toSeq.sortBy(_._1) ) {
  <li><a href="#db@(i)" data-toggle="tab">Database #@(i)</a></li>
  }
  @for( (i, entry) <- other.toSeq.sortBy(_._1) ) {
  <li><a href="#other@(i)" data-toggle="tab">Other #@(i)</a></li>
  }
</ul>
<div class="tab-content">
  <div class="tab-pane active" id="inequality">
    <div class="row" style="margin-top:20px"></div>
    <div class="panel">
      <div class="panel-heading">
	<h3 class="panel-title">Coefficients</h3>
      </div>
      <ul class="nav nav-tabs">
	@for((notation, index) <- vvh.notations.zipWithIndex) {
	  <li class="@(if (index == 0) "active" else "")">
	    <a href="#coeffs-@(notation.id)" data-toggle="tab">@(notation.title)</a>
	  </li>
	}
      </ul>
      <div class="tab-content">
	@for((notation, index) <- vvh.notations.zipWithIndex) {
	<div class="tab-pane@(if (index == 0) " active" else "")" id="coeffs-@(notation.id)" style="@(notation.style)">
	  @(notation.content)
        </div>
	}
      </div>
    </div>
  </div>
  <div class="tab-pane" id="decomposition">
    <pre class="brush: yaml">
      @(decomposition)
    </pre>
  </div>
  @for( (index, ce) <- recognized) {
  <div class="tab-pane" id="db@(index)">
    <div class="row" style="margin-top:20px"></div>
    @path(ce.path)
    @row1(ce)
    @row2(ce, new VecViewHelper(ce.inequality.bra), "db" + index)
    @row3(ce)
  </div>
  }
  @for( (index, vvh1) <- other) {
  <div class="tab-pane" id="other@(index)">
    <div class="row" style="margin-top:20px"></div>
    <div class="row">
      <div class="col-lg-2">
	<div class="panel">
	  <div class="panel-heading">
	    <h3 class="panel-title">Scenario</h3>
	  </div>
	  @(vvh1.vec.scenario.toString)
	</div>
      </div>
      <div class="col-lg-10">
	<div class="panel">
	  <div class="panel-heading">
	    <h3 class="panel-title">Coefficients</h3>
	  </div>
	  @vvhview(vvh1, s"other$index")
	</div>
      </div>
    </div>
  </div>
  }
  
</div>
}
