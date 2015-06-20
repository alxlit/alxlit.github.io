---
title: alxlit
nav_title: Blog
layout: default
---

# Blog

<ul class="posts">
  {% for post in site.posts %}
    <li>
      <aside><p>{{ post.date | date_to_string }}</p></aside>
      <a href="{{ post.url }}">{{ post.title }}</a>
    </li>
  {% endfor %}
</ul>

