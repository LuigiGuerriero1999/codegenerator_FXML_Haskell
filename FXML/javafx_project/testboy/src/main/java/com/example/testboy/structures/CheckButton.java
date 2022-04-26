package com.example.testboy.structures;

import java.util.Objects;

public class CheckButton extends GTKWidget{
    private String text;

    public CheckButton(String text) {
        this.text = text;
    }

    public CheckButton(String id, Integer id_hash, String name, double layoutX, double layoutY, String text) {
        super(id, id_hash, makeName(id, id_hash, name), layoutX, layoutY);
        this.text = text;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    @Override
    public String gtkHsCode(){
        String checkButtonGtkHsCode = super.getName() + " <- Gtk.checkButtonNew\n  ";
        if (!Objects.equals(text, "")) checkButtonGtkHsCode = super.getName() + " <- Gtk.checkButtonNewWithLabel "+"\""+text+"\""+"\n  ";
        return checkButtonGtkHsCode + "\n  ";
    }
}
