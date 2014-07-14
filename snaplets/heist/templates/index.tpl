<apply template="base">
<header id="jump">
  <div className="sections pull-left">
    <a href="/">
      <i class="fa fa-home"></i>
    </a>
  </div>
</header>

  <div id="splash">
    <h1 class="demo-logo">
        Mouser Match
        <small>Lorem Ipsum</small>
      </h1>
  </div>

<div class="container">
  <div class="row">
    <div class="col-md-8 col-md-offset-2 main">
      <ifLoggedIn>
        <p>Congrats!  You're logged in as '<loggedInUser/>'</p>
        <p><a href="/logout">Logout</a></p>
      </ifLoggedIn>

      <ifLoggedOut>
        <apply template="_login"/>
      </ifLoggedOut>
    </div>
  </div>
</div>
</apply>
