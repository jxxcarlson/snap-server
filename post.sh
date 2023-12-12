curl -X POST \
     -H "Origin: http://localhost:8000" \
     -H "Content-Type: application/json" \
     -d '{"path": "data/example.txt", "content": "Sample text"}' \
     http://localhost:8080/postdata -i
