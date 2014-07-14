<apply template="base">
    <div class="container">
      <div class="row">
        <div class="col-md-12">
            Header
        </div>
      </div>
      <div class="row">
        <div class="col-md-12">

            <h1> Add a new cat! </h1>
            <div class="form">
                <dfForm action="/cat">
                  <dfChildErrorList ref="" />
                  <apply template="_cat_data"/>
                  <dfInputSubmit />
                </dfForm>
            </div>
        </div>
      </div>
    </div>
</apply>
