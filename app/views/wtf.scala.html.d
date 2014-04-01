@(wtfForm:Form[WTF], syntax: Html)

@import helper._

@main("What's the faacet ?") {
  <div class="row" style="margin-top:20px"></div>
  <div class="row">
    <div class="col-lg-8">
      <div class="panel">
	<div class="panel-heading">
	  <h3 class="panel-title">What's the faacet?</h3>
	</div>
	@helper.form(action = routes.Application.wtfPost) {
	  @inputText(wtfForm("scenario"), 'class -> "form-control")
	  @textarea(wtfForm("coeffs"), 'class -> "form-control", 'style -> "min-height: 200px")
	<input type="submit" value="Submit"/>
	}
      </div>
    </div>
    <div class="col-lg-4">
      <div class="panel">
	<div class="panel-heading">
	  <h3 class="panel-title">Try these...</h3>
	</div>
	<a href="#" onclick="$('#scenario').val('[(2 2) (2 2)]'); $('#coeffs').val('<A1 B1> + <A2 B1> + <A1 B2> - <A2 B2>');">
	  CHSH
	</a>
	<a href="#" onclick="$('#scenario').val('[(3 3) (3 3)]'); $('#coeffs').val('P_A(1|1) + P_B(1|1) - P_AB(1,1|1,1) - P_AB(1,1|2,1) - P_AB(2,1|2,1) + P_AB(2,2|1,1) - P_AB(2,2|2,1) - P_AB(1,1|1,2) + P_AB(2,1|2,2) - P_AB(1,2|1,2) - P_AB(2,2|1,2) + P_AB(1,2|2,2) + P_AB(2,2|2,2)');">
	  CGMLP
	</a>
	<a href="#" onclick="$('#scenario').val('[(2 2) (2 2) (2 2)]'); $('#coeffs').val('1/4 P(111|111) + 1/4 P(212|221) + 1/4 P(221|122) + 1/4 P(122|212)');">
	  Guess Your Neighbor's Input
	</a>
      </div>
      <div class="panel">
	<div class="panel-heading">
	  <h3 class="panel-title">Syntax</h3>
	</div>
	@syntax
      </div>
    </div>
  </div>
}
