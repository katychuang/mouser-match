<h1> Editing a cat! </h1>


<dfForm action="/cat/${id}">
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
</dfForm>
