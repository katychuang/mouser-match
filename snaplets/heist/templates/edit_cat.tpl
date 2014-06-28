<h1> Editing a cat! </h1>


<dfForm action="/cat/${id}">
  <dfChildErrorList ref="" />

  <dfInputHidden ref="id"/>
  <dfSubView ref="catData">
    <apply template="_cat_data"/>
  </dfSubView>
  <input type="hidden" name="_method" value="put" />
  <dfInputSubmit />
</dfForm>
