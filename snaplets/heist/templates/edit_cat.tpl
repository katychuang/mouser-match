<h1> Editing a cat! </h1>


<dfForm action="/cat/${id}">
  <dfChildErrorList ref="" />

  <dfLabel ref="name">Cat Name: </dfLabel>
  <dfInputText ref="name" value="${name}"/>
  <br/>

  <dfLabel ref="ownerName">Owner Name: </dfLabel>
  <dfInputText ref="ownerName" value="${ownerName}"/>
  <br/>

  <dfLabel ref="temperament">Temperament: </dfLabel>
  <dfInputSelect ref="temperament" />
  <br/>

  <dfLabel ref="about">About: </dfLabel>
  <textarea name="form.about"><about/></textarea>
  <br/>

  <input type="hidden" name="_method" value="put" />
  <dfInputSubmit />
</dfForm>
