<form method="post" action="${postAction}" role="form" >

    <div class="form-group">
      <div class="col-md-3">
        <label class="control-label">Login:</label>
      </div>
      <div class="col-md-9">
        <input type="text" name="login" size="20" class="form-control"/>
      </div>
    </div>
    <div class="form-group">
      <div class="col-md-3">
        <label class="control-label">Password:</label>
      </div>
      <div class="col-md-9">
        <input type="password" name="password" size="20"  class="form-control"/>
      </div>
    </div>

    <div class="control-group col-md-12">
      <input type="submit" value="${submitText}" class="btn btn-primary btn-large" style="magin-left:50%" />
    </div>

</form>
