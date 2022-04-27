package com.example.testboy.relations;

public class NotebookRelation extends Relation{
    String labelContent;

    public NotebookRelation(String parentName, String childName, String labelcontent) {
        super(parentName, childName);
        this.labelContent = labelcontent;
    }

    public String getLabelContent() {
        return labelContent;
    }

    public void setLabelContent(String labelContent) {
        this.labelContent = labelContent;
    }

    @Override
    public String generateGtkHsCode(){
        String haskellCode = "Gtk.notebookAppendPage "+getParentName()+" "+getChildName()+" (Just "+labelContent+")\n  ";
        return haskellCode;
    }
}