<h1> Add a new cat! </h1>

<form method="post" action="/cat">
  <fieldset>
    cat name   : <input type="text" name="createCat.name"><br/>
    owner name : <input type="text" name="createCat.ownerName"><br/>
    temperament: <select name="createCat.temperament">
      <option value="createCat.temperament.0">friendly</option>
      <option value="createCat.temperament.1">shy</option>
      <option value="createCat.temperament.2">fiery</option>
    </select><br/>
    about : <textarea name="createCat.about"/><br/>
    <input type="submit" value="submit">
</fieldset>
</form>
