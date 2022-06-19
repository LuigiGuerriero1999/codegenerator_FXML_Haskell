package com.example.codegenerator.structures;

import main.java.com.example.codegenerator.StringFormat;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

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
        StringBuilder template = new StringBuilder();
        template.append("${NOTEBOOKNAME} <- Gtk.notebookNew \n  ");
        template.append("${NOTEBOOKCONTAINERNAME}  <- Gtk.boxNew OrientationHorizontal 1\n  ");
        template.append("Gtk.set ${NOTEBOOKCONTAINERNAME} [Gtk.widgetWidthRequest :=${WIDTH},  Gtk.widgetHeightRequest :=${HEIGHT}]\n  ");
        template.append("Gtk.boxPackStart ${NOTEBOOKCONTAINERNAME} ${NOTEBOOKNAME} True True 0\n   \n  ");

        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("NOTEBOOKCONTAINERNAME", super.getName());
        toInsert.put("WIDTH", (int)width);
        toInsert.put("HEIGHT", (int)height);
        toInsert.put("NOTEBOOKNAME", notebookName);

        return StringFormat.format(template.toString(), toInsert);
    }
}