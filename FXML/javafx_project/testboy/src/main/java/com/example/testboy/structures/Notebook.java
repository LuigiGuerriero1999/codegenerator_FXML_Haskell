package com.example.testboy.structures;

import java.util.ArrayList;
import java.util.Objects;

public class Notebook extends GTKWidget{
    private int width;
    private int height;

    private String notebookName;

    private ArrayList<Label> notebookLabels;
    public Notebook(String id, Integer id_hash, String name, double layoutX, double layoutY, int width, int height, ArrayList<Label> notebookLabels) {
        super(id, id_hash, makeName(id,id_hash,name)+"Container", layoutX, layoutY);
        this.width = width;
        this.height = height;
        this.notebookName = makeName(id,id_hash,name);
        this.notebookLabels = notebookLabels;
    }

    public ArrayList<Label> getNotebookLabels() {
        return notebookLabels;
    }

    public void setNotebookLabels(ArrayList<Label> notebookLabels) {
        this.notebookLabels = notebookLabels;
    }

    public int getWidth() {
        return width;
    }

    public void setWidth(int width) {
        this.width = width;
    }

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }

    public String getNotebookName() {
        return notebookName;
    }

    public void setNotebookName(String notebookName) {
        this.notebookName = notebookName;
    }

    @Override
    public String gtkHsCode(){
        String notebookConstructor = notebookName + " <- Gtk.notebookNew \n  " ;
        String notebookContainerBoxName = super.getName();
        String createNotebookContainer = notebookContainerBoxName + " <- Gtk.boxNew OrientationHorizontal 1\n  ";
        String setNotebookProperties = "Gtk.set "+notebookContainerBoxName+" [Gtk.widgetWidthRequest := "+(int)width+", Gtk.widgetHeightRequest := "+(int)height+"]\n  ";
        String addNotebookToContainer = "Gtk.boxPackStart "+notebookContainerBoxName+" "+notebookName+" True True 0\n   ";
        String noteBookGtkHsCode = notebookConstructor + createNotebookContainer + setNotebookProperties + addNotebookToContainer + "\n  " ;

        return noteBookGtkHsCode;
    }
}