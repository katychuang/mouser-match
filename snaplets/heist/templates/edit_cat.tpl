<h1> Editing a cat! </h1>


<form action="/cat/${form.id}" method='POST' enctype='application/x-www-form-urlencoded'>
  <dfChildErrorList ref="" />

  <dfInputHidden ref="id"/>
  <dfSubView ref="catData">
    <dfLabel ref="name">Cat Name: </dfLabel>
    <dfInputText ref="name"/>
    <br/>

    <dfLabel ref="ownerName">Owner Name: </dfLabel>
    <dfInputText ref="ownerName"/>
    <br/>

    <dfLabel ref="temperament">Temperament: </dfLabel>
    <dfInputSelect ref="temperament"/>
    <br/>

    <dfLabel ref="about">About: </dfLabel>
    <dfInputTextArea ref="about" />
    <br/>

  </dfSubView>
  <input type="hidden" name="_method" value="put" />
  <dfInputSubmit />
</form>
