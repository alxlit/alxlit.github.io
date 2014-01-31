---
title: alxlit
nav_title: Home
layout: default
---

# Home

<ul>
  {% for post in site.posts %}
    <li>
      <aside><p>{{ post.date | date_to_string }}</p></aside>
      <a href="{{ post.url }}">{{ post.title }}</a>
    </li>
  {% endfor %}
</ul>