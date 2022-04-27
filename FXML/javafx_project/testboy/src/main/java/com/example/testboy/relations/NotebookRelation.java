package com.example.testboy.relations;

import com.example.testboy.structures.GTKWidget;
import javafx.scene.Node;

import java.util.ArrayList;

public class NotebookRelation extends Relation{
    private String labelName; //labelNaam van de reeds aangemaakt widget
    private ArrayList<GTKWidget> containerList; //voor later de uiteindelijke namen te extracten van de child

    public void correctChildName(){
        for(GTKWidget container: containerList){
            var childName = getChildName();
            var idHash = String.valueOf(container.getId_hash());
            if(childName.equals(idHash)){ //indien de childName (op dit moment nog de hash) gelijk is aan de reeds aangemaakte container Hash -> childName overschrijven met de containerName
                setChildName(container.getName());
            }
        }
    }

    public NotebookRelation(String parentName, String childName, String labelcontent) {
        super(parentName, childName);
        this.labelName = labelcontent;
        containerList = new ArrayList<>();
    }

    public String getLabelName() {
        return labelName;
    }

    public void setLabelName(String labelName) {
        this.labelName = labelName;
    }

    public ArrayList<GTKWidget> getContainerList() {
        return containerList;
    }

    public void setContainerList(ArrayList<GTKWidget> containerList) {
        this.containerList = containerList;
    }

    public String getLabelContent() {
        return labelName;
    }

    public void setLabelContent(String labelContent) {
        this.labelName = labelContent;
    }

    @Override
    public String generateGtkHsCode(){
        String haskellCode = "Gtk.notebookAppendPage "+getParentName()+" "+getChildName()+" (Just "+labelName+")\n  ";
        return haskellCode;
    }
}